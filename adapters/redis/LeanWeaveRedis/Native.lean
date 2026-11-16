namespace LeanWeave.Redis.Native

opaque Handle : Type

@[extern "weave_redis_init"] opaque init : IO Unit
@[extern "weave_redis_start"]
opaque start (host : @& String) (port : UInt16) (query : @& ByteArray) : IO Handle
@[extern "weave_redis_poll"]
opaque poll (handle : @& Handle) (maximum : USize) : IO ByteArray
@[extern "weave_redis_close"]
opaque close (handle : @& Handle) : IO Unit

end LeanWeave.Redis.Native
