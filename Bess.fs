module Bess
open Socket
open BessSerialization
open System.Net
open System.Net.Sockets
open System.Collections.Generic

let default_port = 10514
let default_address = IPAddress.Parse("127.0.0.1")

type Bess(address: IPAddress, port: int) =
  let address = address
  let port = port
  let socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
  let addArgs (arg: Option<obj>) (dict: Dictionary<string, obj>) = 
    match arg with
    | None -> ()
    | Some a1 -> dict.Add("arg", a1); ()

  let receiveResponse() =
    let lenBa : (byte array) = Array.zeroCreate 4
    if (socket.ReceiveSynchronously lenBa) then () 
    else failwith "Could not receive"
    let len = ReadLength lenBa
    let ba : (byte array) = Array.zeroCreate len
    if (socket.ReceiveSynchronously ba) then () 
    else failwith "Could not receive"
    let result = Decode ba
    result

  (* This needs to be this way because to is a reserved keyword in F# *)
  let requestBess (cmd: string) (arg: Option<obj>) =
    let dict = new Dictionary<string, obj>()
    dict.Add("to", (box "softnic"))
    dict.Add("cmd", (box cmd))
    addArgs arg dict 
    let result = (Encode dict) |> socket.SendSynchronously
    if result then () else failwith "Could not send request to BESS"
    receiveResponse()

  (* This needs to be this way because to is a reserved keyword in F# *)
  let requestModule (name: string) (cmd: string) (arg: Option<obj>) =
    let dict = new Dictionary<string, obj>()
    dict.Add("to", (box "module"))
    dict.Add("name", (box name))
    dict.Add("cmd", (box cmd))
    addArgs arg dict
    let result = (Encode dict) |> socket.SendSynchronously
    if result then () else failwith "Could not send request to BESS"
    receiveResponse()

  new() = Bess(default_address, default_port)
  
  member this.Connect () =
    socket.ConnectSynchronously(address, port) 
  
  member this.Reset() =
    requestBess "reset_all" None
  
  member this.PauseAll() =
    requestBess "pause_all" None
  
  member this.ResumeAll() =
    requestBess "resume_all" None
  
  member this.ListDrivers() =
    requestBess "list_drivers" None
  
  member this.ResetPorts() =
    requestBess "reset_ports" None
  
  member this.ListPorts() =
    requestBess "list_ports" None

  member this.CreatePort(?driver:string, ?name:string, ?arg: obj) =
    let dict = new Dictionary<string, obj>()
    let drv = defaultArg driver "PMD"
    dict.Add("driver", drv)
    match name with
    | Some n -> dict.Add("name", n); ()
    | None -> ()
    match arg with
    | Some a -> dict.Add("arg", a); ()
    | None -> ()
    requestBess "create_port" (Some (dict :> obj))
  
  member this.DestroyPort(name:string) =
    requestBess "destroy_port" (Some (box name))
  
  member this.GetPortStats(name:string) =
    requestBess "get_port_stats" (Some (box name))

  member this.ListModules() =
    requestBess "list_modules" None

  member this.ListMclasses() =
    requestBess "list_mclasses" None
  
  member this.ResetModules() =
    requestBess "reset_modules" None

  member this.CreateModule(mclass:string, ?name:string, ?arg: obj) =
    let dict = new Dictionary<string, obj>()
    dict.Add("mclass", mclass)
    match name with
    | Some n -> dict.Add("name", n); ()
    | _ -> ()
    match arg with
    | Some a -> dict.Add("arg", a); ()
    | _ -> ()
    requestBess "create_module" (Some (dict :> obj))

  member this.DestroyModule(name: string) =
    requestBess "destroy_module" (Some (box name))
  
  member this.GetModuleInfo(name: string) =
    requestBess "get_module_info" (Some (box name))

  member this.ConnectModules(m1: string, m2: string, ?gate: int64) =
    let gate = defaultArg gate 0L
    let dict = new Dictionary<string, obj>()
    dict.Add("m1", (box m1))
    dict.Add("m2", (box m2))
    dict.Add("gate", (box gate))
    requestBess "connect_modules" (Some (dict :> obj))

  member this.DisconnectModules(name: string, ?gate: int64) =
    let gate = defaultArg gate 0L
    let dict = new Dictionary<string, obj>()
    dict.Add("name", (box name))
    dict.Add("gate", (box gate))
    requestBess "disconnect_modules" (Some (dict :> obj))
  
  member this.QueryModule(name: string, ?arg: obj) =
    requestModule name "query" arg
    
