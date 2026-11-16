#ifndef LEANWEAVE_PLUGIN_H
#define LEANWEAVE_PLUGIN_H
#include <stddef.h>
#include <stdint.h>

#ifdef _WIN32
#define WEAVE_EXPORT __declspec(dllexport)
#else
#define WEAVE_EXPORT __attribute__((visibility("default")))
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define WEAVE_PLUGIN_ABI 1u
typedef struct { const uint8_t *data; size_t size; } weave_slice;

/* Request slices are owned host bytes valid for this callback only. Copy any
   value retained by the exchange. No socket, reactor or borrowed buffer escapes. */
typedef struct {
    weave_slice method, target, headers_json, body, metadata_json;
} weave_plugin_request;

/* poll: 0 pending, 1 response head, 2 stream data, 3 stream finish, <0 failure.
   A head has status 200..599, JSON [[name,value],...] headers, and either one
   bounded complete body or streaming=1 and an empty body. Chunks contain 1..budget
   bytes. Returned slices remain valid through poll return; the host copies them.
   finish uses empty slices; trailers are not part of ABI v1. All callbacks must
   be bounded/nonblocking and run only on the owner that called on_start. */
typedef struct {
    uint32_t kind, status, streaming;
    weave_slice headers_json, body;
} weave_plugin_event;

typedef struct {
    uint32_t abi;
    size_t structure_size;
    const char *name;
    const char *version;
    int (*on_start)(weave_slice configuration, void **state);
    int (*on_request)(void *state, const weave_plugin_request *, void **exchange);
    int (*poll)(void *state, void *exchange, uint64_t monotonic_ms, size_t budget, weave_plugin_event *);
    void (*cancel)(void *state, void *exchange, uint32_t reason);
    void (*release)(void *state, void *exchange);
    void (*on_shutdown)(void *state);
} weave_plugin_api;

/* Export exactly this entry point. Plugins are trusted native code, not a
   sandbox. Never register global callbacks or retain pointers after shutdown. */
WEAVE_EXPORT const weave_plugin_api *leanweave_plugin_v1(void);

#ifdef __cplusplus
}
#endif
#endif
