import LeanWeavePlugins.Native

namespace LeanWeave.Plugins

structure Config where
  maxLibraryBytes : Nat := 16 * 1024 * 1024
  maxRequestBytes : Nat := 65536
  maxHeaderBytes : Nat := 8192
  maxHeaderCount : Nat := 64
  maxChunkBytes : Nat := 16384
  timeoutMs : Nat := 30000
  streamIdleTimeoutMs : Nat := 30000
  streamLifetimeMs : Nat := 3600000
  configuration : ByteArray := ByteArray.empty

def Config.retainedBytes (config : Config) : Nat :=
  config.maxRequestBytes + config.maxHeaderBytes + config.maxChunkBytes + 1024

private def Config.validate (config : Config) : IO Unit := do
  if config.maxLibraryBytes == 0 || config.maxLibraryBytes > 256 * 1024 * 1024 ||
      config.maxRequestBytes == 0 || config.maxRequestBytes > 16 * 1024 * 1024 ||
      config.maxHeaderBytes == 0 || config.maxHeaderBytes > 65536 ||
      config.maxHeaderCount == 0 || config.maxHeaderCount > 100 ||
      config.maxChunkBytes == 0 || config.maxChunkBytes > 65536 ||
      config.configuration.size > config.maxRequestBytes || config.timeoutMs == 0 ||
      config.timeoutMs > 3600000 || config.streamIdleTimeoutMs == 0 || config.streamLifetimeMs == 0 then
    throw (IO.userError "invalid native plugin bounds")

private def appIO (action : IO α) : AppM α := do
  let attempt : IO (Except AppError α) := do
    try return .ok (← action)
    catch _ => return .error { status := 502,message := "native plugin operation failed" }
  let result ← attempt
  match result with | .ok value => return value | .error error => throw error

private def reasonCode : Stream.CancelReason → UInt32
  | .timeout => 2
  | .shutdown => 3
  | .failed => 4
  | _ => 1

private def headers (bytes : ByteArray) (config : Config) : Except AppError (Array LeanNet.HTTP1.Header) := do
  let fail := fun (_ : String) => ({ status := 502,message := "invalid native plugin headers" } : AppError)
  let some source := String.fromUTF8? bytes | throw (fail "")
  let json ← (Lean.Json.parse source).mapError fail
  let pairs ← (Lean.fromJson? json : Except String (Array (String × String))).mapError fail
  if pairs.size > config.maxHeaderCount then throw (fail "")
  for (name,value) in pairs do
    if name.isEmpty || name.utf8ByteSize > 256 || name.toUTF8.data.any (fun b =>
        !((b >= 65 && b <= 90) || (b >= 97 && b <= 122) || (b >= 48 && b <= 57) ||
          [33,35,36,37,38,39,42,43,45,46,94,95,96,124,126].contains b.toNat)) ||
        value.toUTF8.data.any (fun b => (b < 32 && b != 9) || b == 127) ||
        ["content-length","transfer-encoding","connection","upgrade","keep-alive","trailer"].contains name.toLower then
      throw (fail "")
  return pairs.map fun (name,value) => { name,value := value.toUTF8 }

private def handler (library : Native.Library) (config : Config) : Handler := fun context => do
  if context.request.incoming.isSome then throw { status := 400,message := "plugin ABI v1 requires a decoded bounded request body" }
  let method := context.request.method.toUTF8
  let head := (Lean.toJson (context.request.headers.map fun h => (h.name,(String.fromUTF8? h.value).getD ""))).compress.toUTF8
  let metadata := (Lean.Json.mkObj [("tenant",Lean.toJson context.tenant),("requestId",Lean.toJson context.requestId),
    ("traceparent",Lean.toJson (context.header "traceparent" |>.getD "")),("host",Lean.toJson context.host)]).compress.toUTF8
  if method.size + context.request.target.size + head.size + context.request.body.size + metadata.size > config.maxRequestBytes then
    throw { status := 413,message := "plugin request exceeds owned byte bound" }
  let exchange ← appIO (Native.beginRequest library method context.request.target head context.request.body metadata)
  let transferred ← IO.mkRef false
  let close := fun reason => Native.release exchange reason
  let action : AppM Response := do
    let event ← AppM.await (do
      try
        return (← Native.poll exchange (← IO.monoMsNow).toUInt64 config.maxChunkBytes.toUSize).map Except.ok
      catch _ => return some (.error { status := 502,message := "native plugin poll failed" }))
      (fun why => close (reasonCode why)) config.timeoutMs config.retainedBytes
    unless event.kind == 1 do throw { status := 502,message := "native plugin did not return a response" }
    let responseHeaders ← match headers event.headers config with | .ok h => pure h | .error e => throw e
    if event.streaming == 0 then return { status := event.status,headers := responseHeaders,body := event.body }
    let body : Stream.Body := {
      retainedBytes := config.retainedBytes
      idleTimeoutMs := config.streamIdleTimeoutMs,lifetimeMs := config.streamLifetimeMs
      poll := fun budget => do
        match ← Native.poll exchange (← IO.monoMsNow).toUInt64 (min budget config.maxChunkBytes).toUSize with
        | none => return none
        | some event =>
          if event.kind == 2 then return some (.data event.body)
          if event.kind == 3 then return some (.finish #[])
          throw (IO.userError "invalid native plugin stream event")
      close := fun why => close (why.map reasonCode |>.getD 0) }
    transferred.set true
    return Response.streaming body event.status responseHeaders
  action.ensuring (do unless ← transferred.get do close 0)

private def copyBounded (source destination : System.FilePath) (bound : Nat) : IO Unit :=
  IO.FS.withFile source .read fun input =>
    IO.FS.withFile destination .write fun output => do
      let mut total := 0
      repeat
        let bytes ← input.read 65536
        if bytes.isEmpty then break
        total := total + bytes.size
        if total > bound then throw (IO.userError "native plugin library exceeds byte bound")
        output.write bytes

/-- Copy a library into a private generation directory before dlopen/LoadLibrary.
Rebuilding/replacing the same source path therefore loads new code while an old
mapping serves its leased requests. The directory is deleted after actual unload.
Call only through Manager.reload so its capacity check precedes copying/loading. -/
def load (source : System.FilePath) (config : Config := {}) : IO Plugin.Definition := do
  config.validate
  let source ← IO.FS.realPath source
  let directory ← IO.FS.createTempDir
  let name := if System.Platform.isWindows then "plugin.dll" else if System.Platform.isOSX then "plugin.dylib" else "plugin.so"
  let image := directory / name
  let prepare : IO Plugin.Definition := do
    copyBounded source image config.maxLibraryBytes
    let library ← Native.openLibrary image.toString config.maxRequestBytes.toUSize config.maxHeaderBytes.toUSize config.maxChunkBytes.toUSize
    let construct : IO Plugin.Definition := do
      let (name,version) ← Native.identity library
      let shutdown : IO Unit := do
        try Native.close library
        finally IO.FS.removeDirAll directory
      return {
        name,version,onStart := Native.start library config.configuration
        onRequest := handler library config
        onShutdown := shutdown }
    tryCatch construct fun error => do
      Native.close library
      throw error
  tryCatch prepare fun error => do
    IO.FS.removeDirAll directory
    throw error

end LeanWeave.Plugins
