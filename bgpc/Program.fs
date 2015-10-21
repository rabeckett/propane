

let RE = Regex.RegularExpression(Set.ofList ["A"; "B"; "C"; "D"; "X"; "Y"; "M"; "N"], Set.empty)

[<EntryPoint>]
let main argv = 
    (* let r = RE.concat (RE.loc "A") (RE.concat (RE.loc "X") (RE.concat (RE.loc "N") (RE.concat (RE.loc "Y") (RE.loc "B")) )) *)
    (* let r = RE.star RE.inside *)

    let x = RE.Star RE.Inside
    let r1 = RE.Concat (RE.Concat x (RE.Loc "M")) x
    let r2 = RE.Concat (RE.Concat x (RE.Loc "N")) x

    let dfa1 = RE.MakeDFA 1 (RE.Rev r1)
    let dfa2 = RE.MakeDFA 2 (RE.Rev r2)
    
    let cg = ConstraintGraph.build (Topology.Example2.topo()) [|dfa1; dfa2|] 
    
    ConstraintGraph.removeDeadStates cg
    printfn "%s" (ConstraintGraph.toDot cg)

    (* ConstraintGraph.compile cg *)

    0


