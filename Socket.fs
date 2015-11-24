module Socket

open System.Net
open System.Net.Sockets
open System.Collections.Generic

let toIList<'T>(data : 'T array) =
  let segment = new System.ArraySegment<'T>(data)
  let data = new List<System.ArraySegment<'T>>() :> IList<System.ArraySegment<'T>>
  data.Add(segment)
  data

type Socket with
   member this.ConnectAsync(address: IPAddress, port: int) =
     Async.FromBeginEnd(address, port,
                        (fun (address, port, callback, state) ->
                           this.BeginConnect(address, port, callback, state)),
                        this.EndConnect)
   member this.SendAsync(data: byte array, flags: SocketFlags) =
     Async.FromBeginEnd(toIList data, flags,
                        (fun (data: IList<System.ArraySegment<byte>>, flags, callback, state) ->
                           this.BeginSend(data, flags, callback, state)),
                        this.EndSend)
   member this.ReceiveAsync(data, flags: SocketFlags) =
     Async.FromBeginEnd(toIList data, flags,
                        (fun (data: IList<System.ArraySegment<byte>>, flags, callback, state) ->
                           this.BeginReceive(data, flags, callback, state)),
                        this.EndReceive)
   member this.ConnectSynchronously(address, port) = 
     let f = async {
       do! this.ConnectAsync(address, port)
     }
     let ret = f |> Async.Catch |> Async.RunSynchronously
     match ret with
       | Choice1Of2 _ -> true
       | Choice2Of2 e -> raise (e) 

   member this.SendSynchronously(data) = 
     let f = async {
       let! r = this.SendAsync(data, SocketFlags.None)
       return r
     }
     let ret = f |> Async.Catch |> Async.RunSynchronously
     match ret with
       | Choice1Of2 _ -> true
       | Choice2Of2 e -> raise (e) 
