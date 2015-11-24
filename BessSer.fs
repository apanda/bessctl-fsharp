module BessSerialization
open System.Collections.Generic
open System
open System.Reflection

(* TODO: This only works for little endian right now, add support for running on
 * big endian machines *)
[<AttributeUsage(AttributeTargets.Class, AllowMultiple=false)>]
type BessMessage () =
  class
  inherit Attribute()
end

(* Frame messages before sending them to BESS by adding length *)
let private addLength (data: byte array) =
  Array.concat [|(System.BitConverter.GetBytes (int data.Length)); 
                 data |]

let ReadLength (data: byte array) =
  System.BitConverter.ToInt32(data, 0)

(* Add BESS CTL channel messge *)
let private encode (t: int) (len: int) (data : byte array) =
  Array.concat [|(System.BitConverter.GetBytes t);
                 (System.BitConverter.GetBytes len);
                 data|]

(* BESS strings need to be 8 byte padded, figure out how many bytes to add *)
let private find8BytePadding (length: int) =
  let l = length
  let m = l % 8
  let p = (8 - m)
  if m = 0 then 0 else p

(* Pad by 8 bytes *)
let private pad8Bytes (a: byte array) =
  let p = find8BytePadding(a.Length + 1) + 1
  Array.append a (Array.zeroCreate p)

(* Some active patterns *)
let (|IsList|_|) (candidate: obj) =
  let t = candidate.GetType()
  if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>>
  then Some (candidate :?> System.Collections.IEnumerable)
  else None

let (|IsMap|_|) (candidate: obj) =
  let t = candidate.GetType()
  if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Dictionary<string, _>>
  then Some(candidate :?> System.Collections.IDictionary)
  else None

let (|IsBessMessage|_|) (candidate: obj) =
  let t = candidate.GetType()
  match t.GetCustomAttributes(typedefof<BessMessage>, true) with
  | [||] -> None
  | _ -> Some(candidate)

(* Constants for encoding *)
[<Literal>]
let NilType = 0l
[<Literal>]
let NilLength = 0l

[<Literal>]
let IntType = 1l
[<Literal>]
let IntLength = 8l

[<Literal>]
let DoubleType = 2l
[<Literal>]
let DoubleLength = 8l

[<Literal>]
let StringType = 3l

[<Literal>]
let BlobType = 4l

[<Literal>]
let ListType = 5l

[<Literal>]
let MapType = 6l

(* Encode NIL type. This should really just be a constant *)
let private encodeNone () =
  encode NilType NilLength [||]

(* Encode integer *)
let private encodeInt (a: int64) =
  encode IntType IntLength (System.BitConverter.GetBytes a)
  
(* Encode double *)
let private encodeDouble (a: double) =
  encode DoubleType DoubleLength (System.BitConverter.GetBytes a)

let private encodeString (a: string) =
  (System.Text.Encoding.ASCII.GetBytes a 
  |> pad8Bytes 
  |> encode StringType a.Length)
  
let private encodeBlob (a: byte array) =
  pad8Bytes a
  |> encode BlobType a.Length

let rec private encodeObj enc =
  match box enc with
  | :? int64 as num -> encodeInt num
  | :? double as dbl -> encodeDouble dbl
  | :? string as str -> encodeString str
  | :? (byte array) as blob -> encodeBlob blob
  | null -> encodeNone ()
  | IsList lst-> encodeList lst
  | IsMap map -> encodeMap map
  | IsBessMessage o -> encodeObject o
  | _ -> failwith (sprintf "Cannot serialize type '%s'" (enc.GetType().Name))
and private encodeList (a: System.Collections.IEnumerable) =
  let asSeq = a |> Seq.cast<obj> 
  asSeq 
  |> Seq.map encodeObj
  |> Array.concat
  |> encode ListType (Seq.length asSeq)
and private strBytes (s:string) = 
  System.Text.Encoding.ASCII.GetBytes s
  |> pad8Bytes
and private encodeMap (a: System.Collections.IDictionary) =
  let kseq = a.Keys |> Seq.cast<string>
  let kbytes = Seq.map strBytes kseq
  let vobjs = a.Values |> Seq.cast<obj> |> Seq.map encodeObj
  Seq.zip kbytes vobjs
  |> Seq.collect (fun x -> [(fst x); (snd x)])
  |> Array.concat
  |> encode MapType (Seq.length kseq)
and private encodeObject (o: obj) =
  let properties = o.GetType().GetProperties() |> 
                   Array.filter (fun x -> x.CanRead)
  properties 
  |> Array.map (fun prop ->
    let value = prop.GetValue(o, null)
    [(strBytes prop.Name); (encodeObj value)])
  |> Seq.collect (fun x -> x)
  |> Array.concat
  |> encode MapType (Seq.length properties)

let Encode encode =
  encodeObj encode 
  |> addLength

(* Decoding *)
let private getTypeAndLength (bstream: byte array) =
  let t = System.BitConverter.ToInt32(bstream, 0)
  let l = System.BitConverter.ToInt32(bstream, 4)
  (t, l)
  
let DecodeInt (bstream: byte array) =
  let (t, l) = getTypeAndLength bstream
  if t = IntType && l = IntLength then System.BitConverter.ToInt64(bstream, 8)
  else failwith "Bad type and length, not an integer"

let DecodeDouble (bstream: byte array) =
  let (t, l) = getTypeAndLength bstream
  if t = DoubleType && l = DoubleLength then System.BitConverter.ToDouble(bstream, 8)
  else failwith "Bad type and length, not a double"

let private decodeStringLen (bstream: byte array) =
  let (t, l) = getTypeAndLength bstream
  if t = StringType then
    let s = System.Text.Encoding.ASCII.GetString(bstream, 8, l)
    let p = find8BytePadding l
    (8 + l + p, (box s))
  else failwith "Bad type and length, not an ASCII string"

let DecodeString (bstream: byte array) =
  let (_, b) = (decodeStringLen bstream)
  unbox b

let private decodeBlobLen (bstream: byte array) =
  let (t, l) = getTypeAndLength bstream
  if t = BlobType then (8 + l, (box bstream.[8..(8+l)]))
  else failwith "Not a byte stream"

let DecodeBlob (bstream: byte array) =
  let (_, b) = (decodeBlobLen bstream)
  unbox b

let rec private decodeWithLength (bstream: byte array) =
  let t = System.BitConverter.ToInt32(bstream, 0)
  match t with 
  | 0 -> (8, null)
  | 1 -> (16, box (DecodeInt bstream)) // boxing since allow normal types too :(
  | 2 -> (16, box (DecodeDouble bstream))
  | 3 -> decodeStringLen bstream
  | 4 -> decodeBlobLen bstream
  | 5 -> decodeListLen bstream
  | 6 -> decodeMapLen bstream
  | _ -> failwith (sprintf "Unsupported type %d" t)
and private decodeListLen (bstream: byte array) =
  let (t, l) = getTypeAndLength bstream
  if t = ListType then
    let arr : (obj array) = Array.zeroCreate l
    let mutable cursor = 8
    for entry = 0 to (l - 1) do
      let (l, b) = decodeWithLength bstream.[cursor..]
      cursor <- cursor + l
      arr.[entry] <- b
    let lst =  Array.toList arr
    (cursor, (box lst))
  else failwith "Not a list"
and private mapKeyLength (bstream: byte array) =
  // Find the null terminator for the string
  let nullTerm = Seq.findIndex (fun x -> x = 0uy) bstream
  // Round up to 8 byte boundary
  let p = find8BytePadding nullTerm
  (nullTerm, nullTerm + p)
and private decodeMapLen (bstream: byte array) =
  let (t, l) = getTypeAndLength bstream
  if t = MapType then
    let dict = Dictionary<string, obj>()
    let mutable cursor  = 8
    for entry = 0 to (l - 1) do
      // First decode string key
      let (slen, klen)  = mapKeyLength bstream.[cursor..]
      let key = System.Text.Encoding.ASCII.GetString(bstream, cursor, slen)
      cursor <- cursor + klen
      let (l, value) = decodeWithLength bstream.[cursor..]
      cursor <- cursor + l
      dict.Add(key, value)
    (cursor, (box dict))
  else failwith "Not a map"

let DecodeList (bstream: byte array) =
  let (_, b) = (decodeListLen bstream)
  unbox b

let DecodeMap (bstream: byte array) =
  let (_, b) = (decodeMapLen bstream)
  unbox b

(* Call for untyped decoding of byte array *)
let Decode (bstream: byte array) : obj =
  let (_, b) = (decodeWithLength bstream)
  b

(* Decode into an object *)
let DecodeObject<'a when 'a : (new: unit -> 'a)> (bstream: byte array): 'a =
  let t = new 'a()
  let properties = t.GetType().GetProperties() |> 
                   Array.filter (fun x -> x.CanWrite)
  let m : Dictionary<string, obj> = DecodeMap bstream
  Array.iter (fun (prop: PropertyInfo) ->
      let k : string = prop.Name 
      prop.SetValue(t, m.[k])
  ) properties
  t
