module BessSerialization
open System.Collections.Generic
open System

(* TODO: This only works for little endian right now, add support for running on
 * big endian machines *)
[<AttributeUsage(AttributeTargets.Class, AllowMultiple=false)>]
type BessMessage () =
  class
  inherit Attribute()
end

let private addLength (data: byte array) =
  Array.concat [|(System.BitConverter.GetBytes (int data.Length)); 
                 data |]

let private encode (t: int) (len: int) (data : byte array) =
  Array.concat [|(System.BitConverter.GetBytes t);
                 (System.BitConverter.GetBytes len);
                 data|]

let private pad8Bytes (a: byte array) =
  let l = a.Length + 1
  let m = l % 8
  let p = (8 - m) + 1
  if m = 0 then Array.append a (Array.zeroCreate 1) 
  else Array.append a (Array.zeroCreate p)

(* Encode NIL type. This should really just be a constant *)
let private encodeNone () =
  encode 0l 0l [||]

let private encodeInt (a: int64) =
  encode 1l 8l (System.BitConverter.GetBytes a)
  
let private encodeDouble (a: double) =
  encode 2l 8l (System.BitConverter.GetBytes a)

let private encodeString (a: string) =
  (System.Text.Encoding.ASCII.GetBytes a 
  |> pad8Bytes 
  |> encode 3l a.Length)
  
let private encodeBlob (a: byte array) =
  pad8Bytes a
  |> encode 4l a.Length

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
  | _ -> Some(obj)

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
  |> encode 5 (Seq.length asSeq)
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
  |> encode 6 (Seq.length kseq)
and private encodeObject (o: obj) =
  let properties = o.GetType().GetProperties() |> 
                   Array.filter (fun x -> x.CanRead)
  properties 
  |> Array.map (fun prop ->
    let value = prop.GetValue(o, null)
    [(strBytes prop.Name); (encodeObj value)])
  |> Seq.collect (fun x -> x)
  |> Array.concat
  |> encode 6 (Seq.length properties)

let Encode encode =
  encodeObj encode 
  |> addLength
