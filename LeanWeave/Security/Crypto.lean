import LeanWeave.Core

namespace LeanWeave.Security.Crypto

private def rotate (x : UInt32) (n : Nat) : UInt32 :=
  (x >>> n.toUInt32) ||| (x <<< (32 - n).toUInt32)

private def rounds : Array UInt32 := #[
  0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,0x3956c25b,0x59f111f1,0x923f82a4,0xab1c5ed5,
  0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,
  0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc,0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
  0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967,
  0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,
  0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3,0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
  0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3,
  0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2]

/-- SHA-256 (FIPS 180-4). Callers apply their payload bounds before hashing. -/
def sha256 (input : ByteArray) : ByteArray := Id.run do
  let mut bytes := input.push 128
  let padding := (56 + 64 - bytes.size % 64) % 64
  for _ in [:padding] do bytes := bytes.push 0
  let bits := input.size * 8
  for i in [:8] do bytes := bytes.push ((bits / (256 ^ (7-i))) % 256).toUInt8
  let mut state : Array UInt32 := #[0x6a09e667,0xbb67ae85,0x3c6ef372,0xa54ff53a,0x510e527f,0x9b05688c,0x1f83d9ab,0x5be0cd19]
  for block in [:bytes.size / 64] do
    let mut words : Array UInt32 := Array.emptyWithCapacity 64
    for i in [:16] do
      let offset := block * 64 + i * 4
      words := words.push ((bytes[offset]!.toUInt32 <<< 24) ||| (bytes[offset+1]!.toUInt32 <<< 16) |||
        (bytes[offset+2]!.toUInt32 <<< 8) ||| bytes[offset+3]!.toUInt32)
    for i in [16:64] do
      let x := words[i-15]!
      let y := words[i-2]!
      let s0 := rotate x 7 ^^^ rotate x 18 ^^^ (x >>> 3)
      let s1 := rotate y 17 ^^^ rotate y 19 ^^^ (y >>> 10)
      words := words.push (words[i-16]! + s0 + words[i-7]! + s1)
    let mut a := state[0]!
    let mut b := state[1]!
    let mut c := state[2]!
    let mut d := state[3]!
    let mut e := state[4]!
    let mut f := state[5]!
    let mut g := state[6]!
    let mut h := state[7]!
    for i in [:64] do
      let s1 := rotate e 6 ^^^ rotate e 11 ^^^ rotate e 25
      let choose := (e &&& f) ^^^ ((~~~e) &&& g)
      let t1 := h + s1 + choose + rounds[i]! + words[i]!
      let s0 := rotate a 2 ^^^ rotate a 13 ^^^ rotate a 22
      let majority := (a &&& b) ^^^ (a &&& c) ^^^ (b &&& c)
      let t2 := s0 + majority
      h := g
      g := f
      f := e
      e := d + t1
      d := c
      c := b
      b := a
      a := t1 + t2
    state := #[state[0]!+a,state[1]!+b,state[2]!+c,state[3]!+d,state[4]!+e,state[5]!+f,state[6]!+g,state[7]!+h]
  let mut result := ByteArray.emptyWithCapacity 32
  for word in state do
    for i in [:4] do result := result.push (word >>> ((3-i)*8).toUInt32).toUInt8
  return result

/-- RFC 2104 HMAC using SHA-256. -/
def hmac256 (key input : ByteArray) : ByteArray := Id.run do
  let key := if key.size > 64 then sha256 key else key
  let mut inner := ByteArray.emptyWithCapacity 64
  let mut outer := ByteArray.emptyWithCapacity 64
  for i in [:64] do
    let byte := key[i]?.getD 0
    inner := inner.push (byte ^^^ 0x36)
    outer := outer.push (byte ^^^ 0x5c)
  return sha256 (outer ++ sha256 (inner ++ input))

/-- Explicit algorithm-name alias for integration code. -/
def hmacSha256 (key input : ByteArray) : ByteArray := hmac256 key input

def hex (input : ByteArray) : String := Id.run do
  let digits := "0123456789abcdef".toUTF8
  let mut out := ByteArray.emptyWithCapacity (input.size * 2)
  for b in input do out := out.push digits[b.toNat/16]! |>.push digits[b.toNat%16]!
  return String.fromUTF8! out

/-- Compare public fixed-length authenticators with constant work over their bytes. -/
def equal (a b : ByteArray) : Bool := Id.run do
  if a.size != b.size then return false
  let mut difference : UInt8 := 0
  for i in [:a.size] do difference := difference ||| (a[i]! ^^^ b[i]!)
  return difference == 0

private def alphabet := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".toUTF8

def base64url (bytes : ByteArray) : String := Id.run do
  let mut out := ByteArray.empty
  for i in [: (bytes.size+2)/3] do
    let offset := i*3
    let a := bytes[offset]!.toNat
    let b := (bytes[offset+1]?.getD 0).toNat
    let c := (bytes[offset+2]?.getD 0).toNat
    out := out.push alphabet[a/4]! |>.push alphabet[(a%4)*16+b/16]!
    if offset+1 < bytes.size then out := out.push alphabet[(b%16)*4+c/64]!
    if offset+2 < bytes.size then out := out.push alphabet[c%64]!
  return String.fromUTF8! out

/-- Strict unpadded URL-safe base64. Noncanonical trailing bits are rejected. -/
def unbase64url (text : String) (maxBytes : Nat := 16384) : Except String ByteArray := do
  if text.utf8ByteSize > (maxBytes*4+2)/3 || text.utf8ByteSize % 4 == 1 then throw "invalid base64url length"
  let mut acc := 0
  let mut bits := 0
  let mut out := ByteArray.empty
  for byte in text.toUTF8 do
    let some value := alphabet.data.findIdx? (· == byte) | throw "invalid base64url alphabet"
    acc := acc * 64 + value
    bits := bits + 6
    if bits >= 8 then
      bits := bits - 8
      out := out.push ((acc / (2^bits)) % 256).toUInt8
      acc := acc % (2^bits)
  if acc != 0 || out.size > maxBytes then throw "noncanonical base64url"
  return out

end LeanWeave.Security.Crypto
