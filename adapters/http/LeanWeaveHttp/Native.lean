namespace LeanWeave.Http.Native
opaque Handle : Type
structure Result where
  status : Nat
  headers : ByteArray
  body : ByteArray
@[extern "weave_http_init"] opaque init : IO Unit
@[extern "weave_http_validate_ca_blob"] opaque validateCABlob : IO Unit
@[extern "weave_http_start"] opaque start (options : @& Array String) (headers : @& Array String)
  (body caBlob : @& ByteArray) (timeoutMs maxBody maxHeaders : USize) : IO Handle
@[extern "weave_http_poll"] opaque poll (handle : @& Handle) : IO (Option Result)
@[extern "weave_http_close"] opaque close (handle : @& Handle) : IO Unit
end LeanWeave.Http.Native
