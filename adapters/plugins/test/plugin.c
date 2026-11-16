#include "leanweave_plugin.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#ifndef VERSION
#define VERSION "v1"
#endif
#ifndef ABI
#define ABI WEAVE_PLUGIN_ABI
#endif
#ifndef FAIL_START
#define FAIL_START 0
#endif

typedef struct { size_t active; } state;
typedef struct { char target[1024];uint64_t started;unsigned phase; } request;
static weave_slice text(const char *s) { weave_slice b={(const uint8_t *)s,strlen(s)};return b; }
static int start(weave_slice configuration,void **result) {
    (void)configuration;
    *result=calloc(1,sizeof(state));
    return !*result || FAIL_START ? -1:0;
}
static int begin(void *value,const weave_plugin_request *input,void **result) {
    state *s=value;
    if(input->target.size>=sizeof(((request *)0)->target)) return -1;
    request *r=calloc(1,sizeof(*r));
    if(!r) return -1;
    memcpy(r->target,input->target.data,input->target.size);
    *result=r;s->active++;
    return 0;
}
static int poll(void *value,void *exchange,uint64_t now,size_t budget,weave_plugin_event *event) {
    (void)value;
    request *r=exchange;
    if(!r->started) r->started=now;
    if(strstr(r->target,"pending")) return 0;
    if(strstr(r->target,"failpoll")) return -1;
    if(strstr(r->target,"slow") && now-r->started<600) return 0;
    int streaming=strstr(r->target,"stream") || strstr(r->target,"forever");
    if(!r->phase) {
        event->kind=1;event->status=200;event->streaming=(uint32_t)streaming;
        event->headers_json=text("[[\"content-type\",\"text/plain\"],[\"x-plugin\",\"" VERSION "\"]]");
        if(strstr(r->target,"badheaders")) event->headers_json=text("[[\"content-length\",\"1\"]]");
        if(strstr(r->target,"badcontrol")) event->headers_json=text("[[\"x-test\",\"\\u0001\"]]");
        if(strstr(r->target,"badnumber")) event->headers_json=text("[[\"x-test\",1e1000000000]]");
        if(strstr(r->target,"badnegative")) event->headers_json=text("[[\"x-test\",1e-1000000000]]");
        if(strstr(r->target,"numericstring")) event->headers_json=text("[[\"x-test\",\"1e1000000000\"]]");
        if(strstr(r->target,"badstatus")) event->status=101;
        if(!streaming) event->body=text(VERSION);
        if(strstr(r->target,"bytes")) { static const uint8_t binary[]={0,255,1,128};event->body.data=binary;event->body.size=sizeof(binary); }
        if(strstr(r->target,"badbody")) { static const uint8_t large[65537]={0};event->body.data=large;event->body.size=sizeof(large); }
        r->phase=1;
        return 1;
    }
    if(r->phase==3) { event->kind=3;return 3; }
    if(!budget) return 0;
    if(r->phase==1) {
        const char *first=VERSION "-first\n";
        if(budget<strlen(first)) return 0;
        event->kind=2;event->body=text(first);r->phase=2;return 2;
    }
    if(strstr(r->target,"forever") || now-r->started<800) return 0;
    if(r->phase==2) {
        const char *last=VERSION "-last\n";
        if(budget<strlen(last)) return 0;
        event->kind=2;event->body=text(last);r->phase=3;return 2;
    }
    event->kind=3;return 3;
}
static void cancel(void *value,void *exchange,uint32_t reason) { (void)value;(void)exchange;(void)reason; }
static void release(void *value,void *exchange) { state *s=value;assert(s->active>0);s->active--;free(exchange); }
static void stop(void *value) { state *s=value;if(s) { assert(s->active==0);free(s); } }
WEAVE_EXPORT const weave_plugin_api *leanweave_plugin_v1(void) {
    static const weave_plugin_api api={ABI,sizeof(weave_plugin_api),"acceptance",VERSION,start,begin,poll,cancel,release,stop};
    return &api;
}
