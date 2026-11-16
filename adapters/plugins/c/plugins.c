#include <lean/lean.h>
#include "leanweave_plugin.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
typedef HMODULE library_handle;
typedef DWORD owner_thread;
static owner_thread current_owner(void) { return GetCurrentThreadId(); }
static int same_owner(owner_thread a, owner_thread b) { return a == b; }
static void unload(library_handle h) { if (h) FreeLibrary(h); }
#else
#include <dlfcn.h>
#include <pthread.h>
typedef void *library_handle;
typedef pthread_t owner_thread;
static owner_thread current_owner(void) { return pthread_self(); }
static int same_owner(owner_thread a, owner_thread b) { return pthread_equal(a,b); }
static void unload(library_handle h) { if (h) dlclose(h); }
#endif

typedef struct {
    library_handle handle;
    const weave_plugin_api *api;
    void *state;
    owner_thread owner;
    size_t requests, max_request, max_headers, max_chunk;
    int started, closed;
} library;

typedef struct {
    library *library;
    lean_object *retained_library;
    void *exchange;
    int released, head, streaming, ended;
} exchange;

static lean_external_class *library_class, *exchange_class;
static lean_obj_res failure(const char *message) {
    return lean_io_result_mk_error(lean_mk_io_error_other_error(0,lean_mk_string(message)));
}
static int on_owner(library *lib) { return same_owner(lib->owner,current_owner()); }

static void release_exchange(exchange *e, uint32_t reason) {
    if (e->released) return;
    if (reason) e->library->api->cancel(e->library->state,e->exchange,reason);
    e->library->api->release(e->library->state,e->exchange);
    e->released = 1;
    e->exchange = NULL;
    if (e->library->requests) e->library->requests--;
}
static void close_library(library *lib) {
    if (lib->closed) return;
    if (lib->started) lib->api->on_shutdown(lib->state);
    lib->state = NULL;
    lib->started = 0;
    unload(lib->handle);
    lib->handle = NULL;
    lib->closed = 1;
}
static void finalize_library(void *data) {
    library *lib = data;
    /* Explicit owner close is required. Never run foreign plugin callbacks on
       a GC thread. Misuse leaks a mapping rather than unloading live code. */
    if (on_owner(lib) && !lib->requests) close_library(lib);
    free(lib);
}
static void visit_library(void *data, b_lean_obj_arg visit) { (void)data; (void)visit; }
static void finalize_exchange(void *data) {
    exchange *e = data;
    if (on_owner(e->library)) release_exchange(e,4);
    lean_dec(e->retained_library);
    free(e);
}
static void visit_exchange(void *data, b_lean_obj_arg visit) {
    exchange *e = data;
    lean_inc(visit); lean_inc(e->retained_library);
    lean_dec(lean_apply_1(visit,e->retained_library));
}
static void initialize_classes(void) {
    library_class = lean_register_external_class(finalize_library,visit_library);
    exchange_class = lean_register_external_class(finalize_exchange,visit_exchange);
}
#ifdef _WIN32
static INIT_ONCE classes_once = INIT_ONCE_STATIC_INIT;
static BOOL CALLBACK initialize_once(PINIT_ONCE x,PVOID y,PVOID *z) {
    (void)x;(void)y;(void)z;initialize_classes();return TRUE;
}
static void ensure_classes(void) { InitOnceExecuteOnce(&classes_once,initialize_once,NULL,NULL); }
#else
static pthread_once_t classes_once = PTHREAD_ONCE_INIT;
static void ensure_classes(void) { pthread_once(&classes_once,initialize_classes); }
#endif

static weave_slice bytes(b_lean_obj_arg value) {
    weave_slice s = { lean_sarray_cptr((lean_object *)value),lean_sarray_size(value) }; return s;
}
static size_t bounded_length(const char *value) {
    size_t n = 0;
    if (!value) return 0;
    while (n <= 128 && value[n]) n++;
    return n;
}
static int valid_slice(weave_slice s,size_t bound) { return s.size <= bound && (s.data || !s.size); }
static lean_object *copy_bytes(weave_slice s) {
    lean_object *result = lean_alloc_sarray(1,s.size,s.size);
    if (s.size) memcpy(lean_sarray_cptr(result),s.data,s.size);
    return result;
}

LEAN_EXPORT lean_obj_res weave_plugin_open(b_lean_obj_arg path,size_t max_request,size_t max_headers,size_t max_chunk,lean_obj_arg world) {
    (void)world;
    ensure_classes();
    if (!max_request || !max_headers || !max_chunk) return failure("invalid native plugin bounds");
    library_handle handle;
    const weave_plugin_api *(*entry)(void) = NULL;
#ifdef _WIN32
    /* Prevent dependency lookup in the current directory. UTF-8 path support
       uses an explicit wide conversion instead of the process ANSI codepage. */
    int n = MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,lean_string_cstr(path),-1,NULL,0);
    if (n <= 0) return failure("invalid plugin UTF-8 path");
    wchar_t *wide = malloc((size_t)n*sizeof(wchar_t));
    if (!wide) return failure("plugin path allocation failed");
    MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,lean_string_cstr(path),-1,wide,n);
    handle = LoadLibraryExW(wide,NULL,LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR | LOAD_LIBRARY_SEARCH_DEFAULT_DIRS);
    free(wide);
    if (!handle) return failure("native plugin load failed");
    FARPROC symbol = GetProcAddress(handle,"leanweave_plugin_v1");
#else
    handle = dlopen(lean_string_cstr(path),RTLD_NOW | RTLD_LOCAL);
    if (!handle) return failure("native plugin load failed");
    void *symbol = dlsym(handle,"leanweave_plugin_v1");
#endif
    _Static_assert(sizeof(entry)==sizeof(symbol),"dynamic symbol pointer size");
    memcpy(&entry,&symbol,sizeof(entry));
    if (!entry) { unload(handle); return failure("native plugin entry missing"); }
    const weave_plugin_api *api = entry();
    if (!api || api->abi != WEAVE_PLUGIN_ABI || api->structure_size != sizeof(*api) ||
        !bounded_length(api->name) || bounded_length(api->name)>128 ||
        !bounded_length(api->version) || bounded_length(api->version)>128 ||
        !api->on_start || !api->on_request || !api->poll || !api->cancel || !api->release || !api->on_shutdown) {
        unload(handle); return failure("native plugin ABI mismatch");
    }
    library *lib = calloc(1,sizeof(*lib));
    if (!lib) { unload(handle); return failure("native plugin allocation failed"); }
    lib->handle=handle;lib->api=api;lib->owner=current_owner();
    lib->max_request=max_request;lib->max_headers=max_headers;lib->max_chunk=max_chunk;
    return lean_io_result_mk_ok(lean_alloc_external(library_class,lib));
}
LEAN_EXPORT lean_obj_res weave_plugin_identity(b_lean_obj_arg object,lean_obj_arg world) {
    (void)world;library *lib=lean_get_external_data(object);
    if (!on_owner(lib) || lib->closed) return failure("plugin is closed or on another owner");
    lean_object *pair=lean_alloc_ctor(0,2,0);
    lean_ctor_set(pair,0,lean_mk_string(lib->api->name));lean_ctor_set(pair,1,lean_mk_string(lib->api->version));
    return lean_io_result_mk_ok(pair);
}
LEAN_EXPORT lean_obj_res weave_plugin_start(b_lean_obj_arg object,b_lean_obj_arg configuration,lean_obj_arg world) {
    (void)world;library *lib=lean_get_external_data(object);
    if (!on_owner(lib) || lib->closed || lib->started) return failure("plugin cannot start on this owner/state");
    if (lean_sarray_size(configuration)>lib->max_request) return failure("plugin configuration exceeds bound");
    /* Shutdown must also clean partially initialized state after start failure. */
    lib->started=1;
    if (lib->api->on_start(bytes(configuration),&lib->state)!=0) return failure("plugin start rejected");
    return lean_io_result_mk_ok(lean_box(0));
}
LEAN_EXPORT lean_obj_res weave_plugin_begin(b_lean_obj_arg object,b_lean_obj_arg method,b_lean_obj_arg target,
    b_lean_obj_arg headers,b_lean_obj_arg body,b_lean_obj_arg metadata,lean_obj_arg world) {
    (void)world;library *lib=lean_get_external_data(object);
    if (!on_owner(lib) || lib->closed || !lib->started) return failure("plugin cannot accept on this owner/state");
    size_t total=0;
    b_lean_obj_arg inputs[]={method,target,headers,body,metadata};
    for(size_t i=0;i<5;i++) {
        size_t n=lean_sarray_size(inputs[i]);
        if(n>lib->max_request-total) return failure("plugin request exceeds bound");
        total+=n;
    }
    exchange *e=calloc(1,sizeof(*e));
    if(!e) return failure("plugin exchange allocation failed");
    weave_plugin_request request={bytes(method),bytes(target),bytes(headers),bytes(body),bytes(metadata)};
    if(lib->api->on_request(lib->state,&request,&e->exchange)!=0) {
        if(e->exchange) { lib->api->cancel(lib->state,e->exchange,4);lib->api->release(lib->state,e->exchange); }
        free(e);return failure("plugin request rejected");
    }
    e->library=lib;e->retained_library=(lean_object *)object;lean_inc(e->retained_library);lib->requests++;
    return lean_io_result_mk_ok(lean_alloc_external(exchange_class,e));
}
LEAN_EXPORT lean_obj_res weave_plugin_poll(b_lean_obj_arg object,uint64_t now,size_t budget,lean_obj_arg world) {
    (void)world;exchange *e=lean_get_external_data(object);library *lib=e->library;
    if(!on_owner(lib) || lib->closed || e->released) return failure("plugin exchange closed or on another owner");
    if(e->ended) return lean_io_result_mk_ok(lean_box(0));
    size_t credit=budget<lib->max_chunk?budget:lib->max_chunk;
    weave_plugin_event event={0};
    int result=lib->api->poll(lib->state,e->exchange,now,credit,&event);
    if(result<0) return failure("plugin poll failed");
    if(!result) return lean_io_result_mk_ok(lean_box(0));
    if(result>3 || event.kind!=(uint32_t)result || !valid_slice(event.headers_json,lib->max_headers) || !valid_slice(event.body,lib->max_chunk)) return failure("plugin output exceeds ABI bounds");
    if(result==1) {
        if(e->head || event.status<200 || event.status>599 || event.streaming>1 || (event.streaming && event.body.size)) return failure("invalid plugin head");
        e->head=1;e->streaming=(int)event.streaming;
    } else if(!e->head || !e->streaming || (result==2 && (!event.body.size || event.body.size>credit)) ||
        event.headers_json.size || (result==3 && event.body.size)) return failure("invalid plugin stream event");
    if(result==3 || (result==1 && !event.streaming)) e->ended=1;
    lean_object *value=lean_alloc_ctor(0,5,0);
    lean_ctor_set(value,0,lean_usize_to_nat((size_t)result));lean_ctor_set(value,1,lean_usize_to_nat(event.status));
    lean_ctor_set(value,2,copy_bytes(event.headers_json));lean_ctor_set(value,3,copy_bytes(event.body));
    lean_ctor_set(value,4,lean_usize_to_nat(event.streaming));
    lean_object *some=lean_alloc_ctor(1,1,0);lean_ctor_set(some,0,value);
    return lean_io_result_mk_ok(some);
}
LEAN_EXPORT lean_obj_res weave_plugin_release(b_lean_obj_arg object,uint32_t reason,lean_obj_arg world) {
    (void)world;exchange *e=lean_get_external_data(object);
    if(!on_owner(e->library)) return failure("plugin exchange moved across owners");
    release_exchange(e,reason);return lean_io_result_mk_ok(lean_box(0));
}
LEAN_EXPORT lean_obj_res weave_plugin_close(b_lean_obj_arg object,lean_obj_arg world) {
    (void)world;library *lib=lean_get_external_data(object);
    if(!on_owner(lib)) return failure("plugin library moved across owners");
    if(lib->requests) return failure("plugin library still has leased exchanges");
    close_library(lib);return lean_io_result_mk_ok(lean_box(0));
}
