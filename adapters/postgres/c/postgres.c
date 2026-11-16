#include <lean/lean.h>
#include <libpq-fe.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
typedef DWORD weave_thread;
static weave_thread current_thread(void) { return GetCurrentThreadId(); }
static int same_thread(weave_thread a, weave_thread b) { return a == b; }
#else
#include <pthread.h>
typedef pthread_t weave_thread;
static weave_thread current_thread(void) { return pthread_self(); }
static int same_thread(weave_thread a, weave_thread b) { return pthread_equal(a, b); }
#endif

typedef struct {
    PGconn *connection;
    weave_thread owner;
    int connected;
    int busy;
    size_t max_rows, max_bytes, max_columns, bytes;
    lean_object *columns;
    lean_object *rows;
    lean_object *command;
} weave_pg;

static lean_external_class *handle_class;

/* libpq's default receiver performs synchronous stderr writes. Application
   error paths expose bounded SQLSTATE data; unsolicited notices are discarded. */
static void discard_notice(void *argument, const PGresult *notice) {
    (void)argument; (void)notice;
}

static void clear_result(weave_pg *handle) {
    lean_dec(handle->columns);
    lean_dec(handle->rows);
    lean_dec(handle->command);
    handle->columns = lean_alloc_array(0, 0);
    handle->rows = lean_alloc_array(0, 0);
    handle->command = lean_mk_string("");
    handle->bytes = 0;
}

static void close_connection(weave_pg *handle) {
    if (handle->connection) PQfinish(handle->connection);
    handle->connection = NULL;
    handle->connected = 0;
    handle->busy = 0;
}

static void finalize_handle(void *data) {
    weave_pg *handle = data;
    close_connection(handle);
    lean_dec(handle->columns);
    lean_dec(handle->rows);
    lean_dec(handle->command);
    free(handle);
}

static void visit_handle(void *data, b_lean_obj_arg visit) {
    weave_pg *handle = data;
    lean_inc(visit); lean_inc(handle->columns);
    lean_dec(lean_apply_1(visit, handle->columns));
    lean_inc(visit); lean_inc(handle->rows);
    lean_dec(lean_apply_1(visit, handle->rows));
    lean_inc(visit); lean_inc(handle->command);
    lean_dec(lean_apply_1(visit, handle->command));
}

static lean_obj_res failure(const char *message) {
    return lean_io_result_mk_error(lean_mk_io_error_other_error(0, lean_mk_string(message)));
}

static weave_pg *get_handle(b_lean_obj_arg object) {
    weave_pg *handle = lean_get_external_data(object);
    if (!same_thread(handle->owner, current_thread())) return NULL;
    return handle;
}

static lean_obj_res failed_connection(weave_pg *handle, const char *message) {
    close_connection(handle);
    clear_result(handle);
    return failure(message);
}

static void initialize_handle_class(void) {
    handle_class = lean_register_external_class(finalize_handle, visit_handle);
}

#ifdef _WIN32
static INIT_ONCE handle_once = INIT_ONCE_STATIC_INIT;
static BOOL CALLBACK initialize_once(PINIT_ONCE once, PVOID parameter, PVOID *context) {
    (void)once; (void)parameter; (void)context;
    initialize_handle_class(); return TRUE;
}
static void ensure_class(void) { InitOnceExecuteOnce(&handle_once, initialize_once, NULL, NULL); }
#else
static pthread_once_t handle_once = PTHREAD_ONCE_INIT;
static void ensure_class(void) { pthread_once(&handle_once, initialize_handle_class); }
#endif

LEAN_EXPORT lean_obj_res weave_pg_start(b_lean_obj_arg options, lean_obj_arg world) {
    (void)world;
    ensure_class();
    if (lean_array_size(options) != 11) return failure("invalid PostgreSQL options");
    const char *keys[] = {"hostaddr", "host", "port", "dbname", "user", "password", "sslmode",
        "application_name", "options", "client_encoding", "sslrootcert", "gssencmode", NULL};
    const char *values[13];
    for (size_t index = 0; index < 11; index++)
        values[index] = lean_string_cstr(lean_array_get_core(options, index));
    values[11] = "disable";
    values[12] = NULL;
    weave_pg *handle = calloc(1, sizeof(*handle));
    if (!handle) return failure("PostgreSQL handle allocation failed");
    handle->owner = current_thread();
    handle->columns = lean_alloc_array(0, 0);
    handle->rows = lean_alloc_array(0, 0);
    handle->command = lean_mk_string("");
    handle->connection = PQconnectStartParams(keys, values, 0);
    if (!handle->connection || PQstatus(handle->connection) == CONNECTION_BAD) {
        finalize_handle(handle);
        return failure("PostgreSQL connection could not start");
    }
    PQsetNoticeReceiver(handle->connection, discard_notice, NULL);
    return lean_io_result_mk_ok(lean_alloc_external(handle_class, handle));
}

LEAN_EXPORT lean_obj_res weave_pg_connect_poll(b_lean_obj_arg object, lean_obj_arg world) {
    (void)world;
    weave_pg *handle = get_handle(object);
    if (!handle) return failure("PostgreSQL pool moved across worker threads");
    if (!handle->connection) return failure("PostgreSQL connection is closed");
    if (handle->connected) return lean_io_result_mk_ok(lean_box(1));
    PostgresPollingStatusType result = PQconnectPoll(handle->connection);
    if (result == PGRES_POLLING_FAILED)
        return failed_connection(handle, "PostgreSQL connection failed");
    if (result == PGRES_POLLING_OK) {
        if (PQsetnonblocking(handle->connection, 1) != 0)
            return failed_connection(handle, "PostgreSQL nonblocking setup failed");
        handle->connected = 1;
        return lean_io_result_mk_ok(lean_box(1));
    }
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res weave_pg_send(b_lean_obj_arg object, b_lean_obj_arg sql,
        b_lean_obj_arg params, size_t max_rows, size_t max_bytes, size_t max_columns, lean_obj_arg world) {
    (void)world;
    weave_pg *handle = get_handle(object);
    if (!handle) return failure("PostgreSQL pool moved across worker threads");
    if (!handle->connection || !handle->connected || handle->busy)
        return failure("PostgreSQL connection is unavailable");
    size_t count = lean_array_size(params);
    if (count > 65535) return failure("too many PostgreSQL parameters");
    const char **values = count ? calloc(count, sizeof(*values)) : NULL;
    if (count && !values) return failure("PostgreSQL parameter allocation failed");
    for (size_t index = 0; index < count; index++) {
        lean_object *value = lean_array_get_core(params, index);
        values[index] = lean_is_scalar(value) ? NULL : lean_string_cstr(lean_ctor_get(value, 0));
    }
    clear_result(handle);
    handle->max_rows = max_rows;
    handle->max_bytes = max_bytes;
    handle->max_columns = max_columns;
    /* Result/array headers and the bounded final command tag. */
    handle->bytes = 256;
    int sent = PQsendQueryParams(handle->connection, lean_string_cstr(sql), (int)count,
        NULL, values, NULL, NULL, 0);
    free(values);
    if (!sent || !PQsetSingleRowMode(handle->connection))
        return failed_connection(handle, "PostgreSQL parameterized send failed");
    handle->busy = 1;
    return lean_io_result_mk_ok(lean_box(0));
}

static int take_row(weave_pg *handle, PGresult *result) {
    int fields = PQnfields(result);
    if (fields < 0 || (size_t)fields > handle->max_columns) return 0;
    int first = lean_array_size(handle->columns) == 0;
    size_t required = PQntuples(result) ? 64 : 0;
    if (required > handle->max_bytes) return 0;
    for (int column = 0; column < fields; column++) {
        size_t name_bytes = first ? strlen(PQfname(result, column)) : 0;
        if (name_bytes > handle->max_bytes - required) return 0;
        required += name_bytes;
        size_t value_bytes = PQntuples(result) && !PQgetisnull(result, 0, column)
            ? (size_t)PQgetlength(result, 0, column) : 0;
        if (value_bytes > handle->max_bytes - required) return 0;
        required += value_bytes;
        /* Column-name and cell objects are separate allocations on the first row. */
        size_t overhead = (first ? 64u : 0u) + (PQntuples(result) ? 64u : 0u);
        if (overhead > handle->max_bytes - required) return 0;
        required += overhead;
    }
    if (required > handle->max_bytes - handle->bytes) return 0;
    if (PQntuples(result) && lean_array_size(handle->rows) >= handle->max_rows) return 0;
    handle->bytes += required;
    lean_object *row = lean_alloc_array(0, (size_t)fields);
    for (int column = 0; column < fields; column++) {
        if (first) handle->columns = lean_array_push(handle->columns, lean_mk_string(PQfname(result, column)));
        if (PQntuples(result)) {
            lean_object *value = lean_box(0);
            if (!PQgetisnull(result, 0, column)) {
                value = lean_alloc_ctor(1, 1, 0);
                lean_ctor_set(value, 0, lean_mk_string_from_bytes(PQgetvalue(result, 0, column),
                    (size_t)PQgetlength(result, 0, column)));
            }
            row = lean_array_push(row, value);
        }
    }
    if (PQntuples(result)) handle->rows = lean_array_push(handle->rows, row);
    else lean_dec(row);
    return 1;
}

LEAN_EXPORT lean_obj_res weave_pg_poll(b_lean_obj_arg object, lean_obj_arg world) {
    (void)world;
    weave_pg *handle = get_handle(object);
    if (!handle) return failure("PostgreSQL pool moved across worker threads");
    if (!handle->connection || !handle->busy) return failure("PostgreSQL query is not active");
    int flushed = PQflush(handle->connection);
    if (flushed < 0) return failed_connection(handle, "PostgreSQL write failed");
    if (flushed > 0) return lean_io_result_mk_ok(lean_box(0));
    if (!PQconsumeInput(handle->connection)) return failed_connection(handle, "PostgreSQL read failed");
    for (unsigned work = 0; work < 16 && !PQisBusy(handle->connection); work++) {
        PGresult *result = PQgetResult(handle->connection);
        if (!result) {
            handle->busy = 0;
            lean_object *response = lean_alloc_ctor(0, 3, 0);
            lean_inc(handle->columns); lean_ctor_set(response, 0, handle->columns);
            lean_inc(handle->rows); lean_ctor_set(response, 1, handle->rows);
            lean_inc(handle->command); lean_ctor_set(response, 2, handle->command);
            lean_object *some = lean_alloc_ctor(1, 1, 0);
            lean_ctor_set(some, 0, response);
            clear_result(handle);
            return lean_io_result_mk_ok(some);
        }
        ExecStatusType status = PQresultStatus(result);
        if (status != PGRES_SINGLE_TUPLE && status != PGRES_TUPLES_OK && status != PGRES_COMMAND_OK) {
            char message[96];
            const char *state = PQresultErrorField(result, PG_DIAG_SQLSTATE);
            snprintf(message, sizeof(message), "PostgreSQL query failed (SQLSTATE %.5s)", state ? state : "?????");
            PQclear(result);
            return failed_connection(handle, message);
        }
        if ((status == PGRES_SINGLE_TUPLE || status == PGRES_TUPLES_OK) && !take_row(handle, result)) {
            PQclear(result);
            return failed_connection(handle, "PostgreSQL result budget exceeded");
        }
        if (status == PGRES_TUPLES_OK || status == PGRES_COMMAND_OK) {
            const char *tag = PQcmdStatus(result);
            if (strlen(tag) > 128) {
                PQclear(result);
                return failed_connection(handle, "PostgreSQL command tag budget exceeded");
            }
            lean_dec(handle->command);
            handle->command = lean_mk_string(tag);
        }
        PQclear(result);
    }
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res weave_pg_healthy(b_lean_obj_arg object, lean_obj_arg world) {
    (void)world;
    weave_pg *handle = get_handle(object);
    if (!handle) return failure("PostgreSQL pool moved across worker threads");
    int healthy = handle->connection && handle->connected && !handle->busy &&
        PQstatus(handle->connection) == CONNECTION_OK && PQtransactionStatus(handle->connection) == PQTRANS_IDLE;
    return lean_io_result_mk_ok(lean_box((size_t)healthy));
}

LEAN_EXPORT lean_obj_res weave_pg_close(b_lean_obj_arg object, lean_obj_arg world) {
    (void)world;
    weave_pg *handle = get_handle(object);
    if (!handle) return failure("PostgreSQL pool moved across worker threads");
    close_connection(handle);
    clear_result(handle);
    return lean_io_result_mk_ok(lean_box(0));
}
