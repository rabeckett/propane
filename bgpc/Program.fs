open Common
open Common.Debug
open Common.Error


[<EntryPoint>]
let main argv =
    (* Parse command line settings *)
    ignore (Args.parse argv)
    let settings = Args.getSettings ()
    logInfo0 (sprintf "Got settings: %A" settings)

    (* Run unit tests if in test mode *)
    if settings.Test then
        Test.run ()
        exit 0

    (* Set debugging output file *)
    let fileName = Option.getOrDefault "output" settings.OutFile
    let fullName = settings.DebugDir + (string System.IO.Path.DirectorySeparatorChar) + fileName

    (* Get the topology *)
    let topo = Examples.topoDatacenterSmall()

    match settings.PolFile with 
    | None -> error ("No policy file specified")
    | Some p ->
        let ast = Input.readFromFile p
        let pairs = Ast.makePolicyPairs ast topo
        let ir = IR.compileAllPrefixes fullName pairs

        match settings.OutFile with
        | None -> ()
        | Some out -> System.IO.File.WriteAllText(out + ".ir", IR.format ir)

        match settings.Format with 
        | Args.IR -> ()
        | Args.Template -> () (* TODO: another compilation step *)

    0