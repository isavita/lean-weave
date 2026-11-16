import LeanWeave.Plugin

namespace LeanWeave.Plugins.Native

private opaque LibraryImpl : NonemptyType
def Library : Type := LibraryImpl.type
instance : Nonempty Library := LibraryImpl.property
private opaque ExchangeImpl : NonemptyType
def Exchange : Type := ExchangeImpl.type
instance : Nonempty Exchange := ExchangeImpl.property

structure Event where
  kind : Nat
  status : Nat
  headers : ByteArray
  body : ByteArray
  streaming : Nat

@[extern "weave_plugin_open"] opaque openLibrary (path : @& String)
  (maxRequest maxHeaders maxChunk : USize) : IO Library
@[extern "weave_plugin_identity"] opaque identity (library : @& Library) : IO (String × String)
@[extern "weave_plugin_start"] opaque start (library : @& Library) (configuration : @& ByteArray) : IO Unit
@[extern "weave_plugin_begin"] opaque beginRequest (library : @& Library)
  (method target headers body metadata : @& ByteArray) : IO Exchange
@[extern "weave_plugin_poll"] opaque poll (exchange : @& Exchange) (now : UInt64) (budget : USize) : IO (Option Event)
@[extern "weave_plugin_release"] opaque release (exchange : @& Exchange) (reason : UInt32) : IO Unit
@[extern "weave_plugin_close"] opaque close (library : @& Library) : IO Unit

end LeanWeave.Plugins.Native
