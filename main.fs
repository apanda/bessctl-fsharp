module main
open Bess

[<EntryPoint>]
let main args =
  printfn "Hello starting"
  let bess = new Bess()
  bess.Connect() |> ignore
  bess.Reset() |> printfn "Reset Got %A"
  bess.PauseAll() |> printfn "Pause All got %A"
  bess.ListDrivers() |> printfn "List Drivers got %A"
  bess.ListMclasses() |> printfn "List Mclasses got %A"
  bess.CreateModule("Measure", "m0") |> printfn "Create module got %A"
  bess.CreateModule("Timestamp", "t0") |> printfn "Create module got %A"
  bess.ConnectModules("m0", "t0")  |> printfn "Create module got %A"
  bess.ListModules() |> printfn "List Modules got %A"
  bess.GetModuleInfo("m0") |> printfn "Get Module Info got %A"
  bess.DestroyModule("m0") |> printfn "Destroy Modules got %A"
  bess.ResumeAll() |> printf "Resume ALL got %A"
  0
