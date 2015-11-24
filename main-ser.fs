module main
open Socket
open BessSerialization
open System.Net
open System.Net.Sockets
open System.Collections.Generic

[<BessMessage>]
type Person(name: string, age: int64) =
  let mutable _name = name
  let mutable _age = age
  member this.Name with get() = _name and set(value) = _name <- value
  member this.Age with get() = _age and set(value) = _age <- value
  new() = Person("", 0L)
  override this.ToString () =
    sprintf "Name: %s Age: %d" _name _age

let port = 10514
let address = IPAddress.Parse("127.0.0.1")
let type_nil = 0uy
let type_int = 1uy
let example = [|0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy|]
let example2 = [|0uy; 0uy; 0uy; type_int; 8uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 128uy|] 
[<EntryPoint>]
let main args =
  printfn "Hello starting"
  let socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
  (*let connectReturn = connect(socket) |> Async.Catch |> Async.RunSynchronously*)
  try 
    do socket.ConnectSynchronously(address, port) |> ignore
    printfn "Connected successfully"
    let encode = Encode null
    do encode |> socket.SendSynchronously |> ignore
    printfn "Sent successfully"
    let decode = unbox Decode encode.[4..] 
    printfn "Decoded to %A" decode

    let encode = Encode 25L
    do encode |> socket.SendSynchronously |> ignore
    printfn "Sent successfully"
    let decode = unbox Decode encode.[4..]
    printfn "Decoded to %A" decode

    let encode = Encode "hello world"
    do encode |> socket.SendSynchronously |> ignore
    printfn "Sent successfully"
    let decode = unbox Decode encode.[4..]
    printfn "Decoded to %A" decode

    let encode = System.Text.Encoding.ASCII.GetBytes "hello world" |> Encode
    do encode |> socket.SendSynchronously |> ignore
    printfn "Sent successfully"
    let decode = unbox Decode encode.[4..]
    printfn "Decoded to %A" decode

    let encode = Encode [1L; 2L; 3L; 4L]
    do encode |> socket.SendSynchronously |> ignore
    printfn "Sent successfully"
    let decode = unbox Decode encode.[4..]
    printfn "Decoded to %A" decode

    let dict = new Dictionary<string, string>()
    do dict.Add("0123456", "Hello")
    dict.Add("B", "World")
    dict.Add("C", "Food")
    dict.Add("D", "Boo")
    let encode = Encode dict
    do encode |> socket.SendSynchronously |> ignore
    printfn "Sent successfully"
    let decode = unbox Decode encode.[4..]
    printfn "Decoded to %A" decode

    let o = new Person("Aurojit", 20L)
    let encode = Encode o
    do encode |> socket.SendSynchronously |> ignore
    printfn "Sent successfully"
    let decode = unbox Decode encode.[4..]
    printfn "Decoded to %A" decode
    let decodeO : Person = DecodeObject encode.[4..]
    printfn "Decoded to %O" decodeO
  with
    e -> printfn "Caught error %s" (string e)
  0
