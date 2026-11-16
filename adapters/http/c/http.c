#include <lean/lean.h>
#include <curl/curl.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
typedef DWORD owner_thread;
static owner_thread current_thread(void) { return GetCurrentThreadId(); }
static int same_thread(owner_thread a, owner_thread b) { return a == b; }
#else
#include <pthread.h>
typedef pthread_t owner_thread;
static owner_thread current_thread(void) { return pthread_self(); }
static int same_thread(owner_thread a, owner_thread b) { return pthread_equal(a,b); }
#endif

typedef struct { unsigned char *data; size_t size, limit; } buffer;
typedef struct {
    owner_thread owner;
    CURL *easy;
    CURLM *multi;
    struct curl_slist *headers, *resolve;
    buffer body, response_headers;
    int completed, failed;
} transfer;
static lean_external_class *handle_class;
static CURLcode global_result;
static lean_obj_res failure(const char *message) {
    return lean_io_result_mk_error(lean_mk_io_error_other_error(0,lean_mk_string(message)));
}
static void close_transfer(transfer *t) {
    if(t->multi && t->easy) curl_multi_remove_handle(t->multi,t->easy);
    if(t->easy) curl_easy_cleanup(t->easy);
    if(t->multi) curl_multi_cleanup(t->multi);
    curl_slist_free_all(t->headers); curl_slist_free_all(t->resolve);
    t->easy=NULL; t->multi=NULL; t->headers=NULL; t->resolve=NULL;
    free(t->body.data); free(t->response_headers.data);
    t->body.data=NULL; t->response_headers.data=NULL;
    t->body.size=0; t->response_headers.size=0;
}
static void finalize(void *data) { transfer *t=data; close_transfer(t); free(t); }
static void visit(void *data,b_lean_obj_arg visitor) { (void)data; (void)visitor; }
static void initialize(void) {
    global_result=curl_global_init(CURL_GLOBAL_DEFAULT);
    handle_class=lean_register_external_class(finalize,visit);
}
#ifdef _WIN32
static INIT_ONCE once=INIT_ONCE_STATIC_INIT;
static BOOL CALLBACK initialize_once(PINIT_ONCE a,PVOID b,PVOID *c) {
    (void)a;(void)b;(void)c;initialize();return TRUE;
}
static void ensure_init(void) { InitOnceExecuteOnce(&once,initialize_once,NULL,NULL); }
#else
static pthread_once_t once=PTHREAD_ONCE_INIT;
static void ensure_init(void) { pthread_once(&once,initialize); }
#endif
LEAN_EXPORT lean_obj_res weave_http_init(lean_obj_arg world) {
    (void)world;ensure_init();
    if(global_result!=CURLE_OK) return failure("HTTP global initialization failed");
    return lean_io_result_mk_ok(lean_box(0));
}
/* Strict worker-safe trust is deliberately restricted to OpenSSL-compatible
 * backends. Other backends may accept CAINFO_BLOB yet still consult native
 * stores or perform revocation I/O; those require explicit systemTrust instead.
 * In MultiSSL builds, inactive backend names are parenthesized. */
static int supports_ca_blob(void) {
    const curl_version_info_data *version=curl_version_info(CURLVERSION_NOW);
    if(!version || version->version_num<0x074d00 || !version->ssl_version) return 0;
    const char *active=version->ssl_version;
    for(;;) {
        while(*active==' ' || *active=='\t') active++;
        if(*active!='(') break;
        const char *end=strchr(active,')');
        if(!end) return 0;
        active=end+1;
    }
    return strncmp(active,"OpenSSL/",8)==0 || strncmp(active,"LibreSSL/",9)==0 ||
        strncmp(active,"BoringSSL",9)==0 || strncmp(active,"AWS-LC/",7)==0;
}
LEAN_EXPORT lean_obj_res weave_http_validate_ca_blob(lean_obj_arg world) {
    (void)world;ensure_init();
    if(global_result!=CURLE_OK || !supports_ca_blob())
        return failure("strict HTTP CA blobs require an OpenSSL-compatible libcurl backend");
    return lean_io_result_mk_ok(lean_box(0));
}
static size_t append(char *data,size_t size,size_t count,void *argument) {
    buffer *b=argument;
    if(size && count>SIZE_MAX/size) return 0;
    size_t length=size*count;
    if(length>b->limit-b->size) return 0;
    if(!length) return 0;
    unsigned char *next=realloc(b->data,b->size+length);
    if(!next) return 0;
    b->data=next;memcpy(b->data+b->size,data,length);b->size+=length;
    return length;
}
static const char *option(b_lean_obj_arg options,size_t index) {
    return lean_string_cstr(lean_array_get_core(options,index));
}
#define SETOPT(name,value) do { if(curl_easy_setopt(t->easy,name,value)!=CURLE_OK) goto setup_failed; } while(0)
LEAN_EXPORT lean_obj_res weave_http_start(b_lean_obj_arg options,b_lean_obj_arg headers,
        b_lean_obj_arg body,b_lean_obj_arg ca_blob,size_t timeout,size_t max_body,size_t max_headers,lean_obj_arg world) {
    (void)world;ensure_init();
    if(global_result!=CURLE_OK || lean_array_size(options)!=5 || !timeout || !max_body || !max_headers)
        return failure("invalid HTTP client configuration");
    int secure=strncmp(option(options,0),"https://",8)==0;
    int blob_mode=strcmp(option(options,3),"blob")==0;
    int system_trust=strcmp(option(options,3),"system")==0;
    if((secure && !blob_mode && !system_trust) ||
            (blob_mode && (!secure || !lean_sarray_size(ca_blob) ||
                lean_sarray_size(ca_blob)>4*1024*1024 || !supports_ca_blob())) ||
            (!blob_mode && lean_sarray_size(ca_blob)))
        return failure("invalid or unsupported HTTP trust configuration");
    transfer *t=calloc(1,sizeof(*t));
    if(!t) return failure("HTTP transfer allocation failed");
    t->owner=current_thread();t->body.limit=max_body;t->response_headers.limit=max_headers;
    t->easy=curl_easy_init();t->multi=curl_multi_init();
    if(!t->easy || !t->multi) goto setup_failed;
    /* URL, METHOD, exact HOST:PORT:numeric ADDRESS, trust mode, HTTP version. */
    SETOPT(CURLOPT_URL,option(options,0));SETOPT(CURLOPT_CUSTOMREQUEST,option(options,1));
    t->resolve=curl_slist_append(NULL,option(options,2));
    if(!t->resolve) goto setup_failed;
    SETOPT(CURLOPT_RESOLVE,t->resolve);
    SETOPT(CURLOPT_PROTOCOLS_STR,"http,https");SETOPT(CURLOPT_REDIR_PROTOCOLS_STR,"http,https");
    SETOPT(CURLOPT_FOLLOWLOCATION,0L);SETOPT(CURLOPT_MAXREDIRS,0L);
    SETOPT(CURLOPT_PROXY,"");SETOPT(CURLOPT_NOPROXY,"*");
    SETOPT(CURLOPT_NOSIGNAL,1L);SETOPT(CURLOPT_TIMEOUT_MS,(long)timeout);
    SETOPT(CURLOPT_CONNECTTIMEOUT_MS,(long)timeout);
    SETOPT(CURLOPT_SSL_VERIFYPEER,1L);SETOPT(CURLOPT_SSL_VERIFYHOST,2L);
    SETOPT(CURLOPT_SSLVERSION,(long)CURL_SSLVERSION_TLSv1_2);
    if(blob_mode) {
        struct curl_blob blob;
        blob.data=(void *)(uintptr_t)(const void *)lean_sarray_cptr(ca_blob);
        blob.len=lean_sarray_size(ca_blob);blob.flags=CURL_BLOB_COPY;
        /* No fallback to build-time files/directories or native CA providers.
         * libcurl owns a bounded copy; borrowed Lean bytes end at this call. */
        SETOPT(CURLOPT_CAINFO,(char *)NULL);
        SETOPT(CURLOPT_CAPATH,(char *)NULL);
        SETOPT(CURLOPT_SSL_OPTIONS,0L);
        SETOPT(CURLOPT_CAINFO_BLOB,&blob);
    }
    SETOPT(CURLOPT_HTTP_VERSION,(long)(strcmp(option(options,4),"2")==0?CURL_HTTP_VERSION_2TLS:CURL_HTTP_VERSION_1_1));
    SETOPT(CURLOPT_WRITEFUNCTION,append);SETOPT(CURLOPT_WRITEDATA,&t->body);
    SETOPT(CURLOPT_HEADERFUNCTION,append);SETOPT(CURLOPT_HEADERDATA,&t->response_headers);
    SETOPT(CURLOPT_MAXFILESIZE_LARGE,(curl_off_t)max_body);
    t->headers=curl_slist_append(NULL,"Expect:");
    if(!t->headers) goto setup_failed;
    for(size_t i=0;i<lean_array_size(headers);i++) {
        struct curl_slist *next=curl_slist_append(t->headers,lean_string_cstr(lean_array_get_core(headers,i)));
        if(!next) goto setup_failed;
        t->headers=next;
    }
    SETOPT(CURLOPT_HTTPHEADER,t->headers);
    if(lean_sarray_size(body) || strcmp(option(options,1),"POST")==0 || strcmp(option(options,1),"PUT")==0 || strcmp(option(options,1),"PATCH")==0) {
        SETOPT(CURLOPT_POSTFIELDSIZE_LARGE,(curl_off_t)lean_sarray_size(body));
        SETOPT(CURLOPT_COPYPOSTFIELDS,(const char *)lean_sarray_cptr(body));
    }
    if(strcmp(option(options,1),"HEAD")==0) SETOPT(CURLOPT_NOBODY,1L);
    if(curl_multi_setopt(t->multi,CURLMOPT_MAX_TOTAL_CONNECTIONS,1L)!=CURLM_OK) goto setup_failed;
    if(curl_multi_add_handle(t->multi,t->easy)!=CURLM_OK) goto setup_failed;
    return lean_io_result_mk_ok(lean_alloc_external(handle_class,t));
setup_failed:
    finalize(t);return failure("HTTP transfer could not start");
}
static lean_object *copy_bytes(buffer *b) {
    lean_object *out=lean_alloc_sarray(1,b->size,b->size);
    if(b->size) memcpy(lean_sarray_cptr(out),b->data,b->size);
    return out;
}
LEAN_EXPORT lean_obj_res weave_http_poll(b_lean_obj_arg object,lean_obj_arg world) {
    (void)world;transfer *t=lean_get_external_data(object);
    if(!same_thread(t->owner,current_thread())) return failure("HTTP transfer moved across worker threads");
    if(!t->easy || t->completed) return failure("HTTP transfer is closed");
    int running=0;
    if(curl_multi_perform(t->multi,&running)!=CURLM_OK) {
        close_transfer(t);return failure("HTTP transport failed");
    }
    int queued=0;CURLMsg *message;
    while((message=curl_multi_info_read(t->multi,&queued))) {
        if(message->msg!=CURLMSG_DONE) continue;
        t->completed=1;
        if(message->data.result!=CURLE_OK) {
            CURLcode code=message->data.result;close_transfer(t);
            if(code==CURLE_OPERATION_TIMEDOUT) return failure("HTTP transport timeout");
            if(code==CURLE_WRITE_ERROR || code==CURLE_FILESIZE_EXCEEDED) return failure("HTTP response byte limit exceeded");
            return failure("HTTP transport or certificate verification failed");
        }
        long status=0;curl_easy_getinfo(t->easy,CURLINFO_RESPONSE_CODE,&status);
        lean_object *result=lean_alloc_ctor(0,3,0);
        lean_ctor_set(result,0,lean_usize_to_nat((size_t)status));
        lean_ctor_set(result,1,copy_bytes(&t->response_headers));lean_ctor_set(result,2,copy_bytes(&t->body));
        lean_object *some=lean_alloc_ctor(1,1,0);lean_ctor_set(some,0,result);
        return lean_io_result_mk_ok(some);
    }
    return lean_io_result_mk_ok(lean_box(0));
}
LEAN_EXPORT lean_obj_res weave_http_close(b_lean_obj_arg object,lean_obj_arg world) {
    (void)world;transfer *t=lean_get_external_data(object);
    if(!same_thread(t->owner,current_thread())) return failure("HTTP transfer moved across worker threads");
    close_transfer(t);return lean_io_result_mk_ok(lean_box(0));
}
