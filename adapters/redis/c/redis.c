/* Nonblocking numeric-address Redis transport. Every call executes on the
 * application owner worker. No tasks, DNS, reactor descriptors or buffer loans. */
#if defined(__APPLE__)
#define _DARWIN_C_SOURCE 1
#endif
#define _POSIX_C_SOURCE 200809L
#include <lean/lean.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <stdatomic.h>
#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <ws2tcpip.h>
#include <windows.h>
typedef SOCKET weave_fd;
#define WEAVE_BAD INVALID_SOCKET
#define weave_close closesocket
#define weave_error WSAGetLastError()
static int pending(int e) { return e == WSAEWOULDBLOCK || e == WSAEINPROGRESS || e == WSAEALREADY; }
#else
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <sys/socket.h>
#include <unistd.h>
typedef int weave_fd;
#define WEAVE_BAD (-1)
#define weave_close close
#define weave_error errno
static int pending(int e) { return e == EAGAIN || e == EWOULDBLOCK || e == EINPROGRESS || e == EALREADY || e == EINTR; }
#endif
#ifdef MSG_NOSIGNAL
#define WEAVE_SEND MSG_NOSIGNAL
#else
#define WEAVE_SEND 0
#endif

typedef struct {
  weave_fd fd;
  unsigned char *query;
  size_t query_size;
  size_t sent;
  int connecting;
} weave_redis;
static lean_external_class *redis_class;
static atomic_flag redis_init_lock = ATOMIC_FLAG_INIT;

static lean_object *failure(const char *stage, int code) {
  char message[160];
  (void)snprintf(message, sizeof(message), "Redis %s failed (socket error %d)", stage, code);
  return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(message)));
}
static void close_redis(weave_redis *connection) {
  if (connection->fd != WEAVE_BAD) {
    (void)weave_close(connection->fd);
    connection->fd = WEAVE_BAD;
  }
  free(connection->query);
  connection->query = NULL;
}
static void finalize_redis(void *data) {
  weave_redis *connection = (weave_redis *)data;
  close_redis(connection);
  free(connection);
}
static void visit_redis(void *data, b_lean_obj_arg visit) { (void)data; (void)visit; }

LEAN_EXPORT lean_obj_res weave_redis_init(lean_obj_arg world) {
  (void)world;
  while (atomic_flag_test_and_set_explicit(&redis_init_lock, memory_order_acquire)) { }
  if (redis_class != NULL) {
    atomic_flag_clear_explicit(&redis_init_lock, memory_order_release);
    return lean_io_result_mk_ok(lean_box(0));
  }
#if defined(_WIN32)
  WSADATA state;
  int result = WSAStartup(MAKEWORD(2, 2), &state);
  if (result != 0) {
    atomic_flag_clear_explicit(&redis_init_lock, memory_order_release);
    return failure("Winsock startup", result);
  }
#endif
  redis_class = lean_register_external_class(finalize_redis, visit_redis);
  atomic_flag_clear_explicit(&redis_init_lock, memory_order_release);
  return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res weave_redis_start(b_lean_obj_arg host, uint16_t port,
    b_lean_obj_arg query, lean_obj_arg world) {
  (void)world;
  struct sockaddr_storage target;
  memset(&target, 0, sizeof(target));
  int family;
  int length;
  struct sockaddr_in *ipv4 = (struct sockaddr_in *)&target;
  struct sockaddr_in6 *ipv6 = (struct sockaddr_in6 *)&target;
  if (inet_pton(AF_INET, lean_string_cstr(host), &ipv4->sin_addr) == 1) {
    family = AF_INET; length = (int)sizeof(*ipv4);
    ipv4->sin_family = AF_INET; ipv4->sin_port = htons(port);
  } else if (inet_pton(AF_INET6, lean_string_cstr(host), &ipv6->sin6_addr) == 1) {
    family = AF_INET6; length = (int)sizeof(*ipv6);
    ipv6->sin6_family = AF_INET6; ipv6->sin6_port = htons(port);
  } else return failure("numeric host validation", 0);
  weave_fd fd = socket(family, SOCK_STREAM, IPPROTO_TCP);
  if (fd == WEAVE_BAD) return failure("socket", weave_error);
#if defined(_WIN32)
  u_long mode = 1;
  if (ioctlsocket(fd, FIONBIO, &mode) != 0 ||
      !SetHandleInformation((HANDLE)fd, HANDLE_FLAG_INHERIT, 0)) {
    int error = weave_error; (void)weave_close(fd); return failure("nonblocking", error);
  }
#else
  int flags = fcntl(fd, F_GETFL, 0);
  if (flags < 0 || fcntl(fd, F_SETFL, flags | O_NONBLOCK) < 0 ||
      fcntl(fd, F_SETFD, FD_CLOEXEC) < 0) {
    int error = errno; (void)weave_close(fd); return failure("nonblocking", error);
  }
#if defined(SO_NOSIGPIPE)
  int enabled = 1;
  if (setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &enabled, sizeof(enabled)) < 0) {
    int error = errno; (void)weave_close(fd); return failure("SIGPIPE suppression", error);
  }
#endif
#endif
  int result = connect(fd, (struct sockaddr *)&target, length);
  if (result != 0 && !pending(weave_error)) {
    int error = weave_error; (void)weave_close(fd); return failure("connect", error);
  }
  weave_redis *connection = (weave_redis *)calloc(1, sizeof(*connection));
  if (connection == NULL) { (void)weave_close(fd); return failure("allocation", 0); }
  connection->fd = fd;
  connection->query_size = lean_sarray_size(query);
  connection->query = (unsigned char *)malloc(connection->query_size);
  if (connection->query == NULL) { finalize_redis(connection); return failure("query allocation", 0); }
  memcpy(connection->query, lean_sarray_cptr(query), connection->query_size);
  connection->connecting = result != 0;
  return lean_io_result_mk_ok(lean_alloc_external(redis_class, connection));
}

LEAN_EXPORT lean_obj_res weave_redis_poll(b_lean_obj_arg handle, size_t maximum,
    lean_obj_arg world) {
  (void)world;
  weave_redis *connection = (weave_redis *)lean_get_external_data(handle);
  if (connection->fd == WEAVE_BAD) return failure("poll after close", 0);
  if (connection->connecting) {
#if defined(_WIN32)
    WSAPOLLFD item = { connection->fd, POLLWRNORM, 0 };
    int ready = WSAPoll(&item, 1, 0);
#else
    struct pollfd item = { connection->fd, POLLOUT, 0 };
    int ready = poll(&item, 1, 0);
#endif
    if (ready < 0) {
      if (pending(weave_error)) return lean_io_result_mk_ok(lean_alloc_sarray(1, 0, 0));
      return failure("connect poll", weave_error);
    }
    if (ready == 0) return lean_io_result_mk_ok(lean_alloc_sarray(1, 0, 0));
    int error = 0;
#if defined(_WIN32)
    int size = (int)sizeof(error);
    if (getsockopt(connection->fd, SOL_SOCKET, SO_ERROR, (char *)&error, &size) != 0)
#else
    socklen_t size = sizeof(error);
    if (getsockopt(connection->fd, SOL_SOCKET, SO_ERROR, &error, &size) != 0)
#endif
      return failure("connect status", weave_error);
    if (error != 0) return failure("connect completion", error);
    connection->connecting = 0;
  }
  if (connection->sent < connection->query_size) {
    size_t remaining = connection->query_size - connection->sent;
    int count = (int)(remaining < 16384 ? remaining : 16384);
    int sent = (int)send(connection->fd, (const char *)connection->query + connection->sent, count, WEAVE_SEND);
    if (sent < 0) {
      if (pending(weave_error)) return lean_io_result_mk_ok(lean_alloc_sarray(1, 0, 0));
      return failure("send", weave_error);
    }
    if (sent == 0) return failure("zero send", 0);
    connection->sent += (size_t)sent;
    if (connection->sent < connection->query_size)
      return lean_io_result_mk_ok(lean_alloc_sarray(1, 0, 0));
    free(connection->query); connection->query = NULL;
  }
  if (maximum == 0) return failure("response byte budget", 0);
  char buffer[16384];
  int capacity = (int)(maximum < sizeof(buffer) ? maximum : sizeof(buffer));
  int received = (int)recv(connection->fd, buffer, capacity, 0);
  if (received < 0) {
    if (pending(weave_error)) return lean_io_result_mk_ok(lean_alloc_sarray(1, 0, 0));
    return failure("receive", weave_error);
  }
  if (received == 0) return failure("truncated response EOF", 0);
  lean_object *bytes = lean_alloc_sarray(1, (size_t)received, (size_t)received);
  memcpy(lean_sarray_cptr(bytes), buffer, (size_t)received);
  return lean_io_result_mk_ok(bytes);
}

LEAN_EXPORT lean_obj_res weave_redis_close(b_lean_obj_arg handle, lean_obj_arg world) {
  (void)world;
  close_redis((weave_redis *)lean_get_external_data(handle));
  return lean_io_result_mk_ok(lean_box(0));
}
