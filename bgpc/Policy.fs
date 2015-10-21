module Policy


type private Prefix = string


type private Constraint = 
    | PathSelection of Prefix * Regex.T list 
    | RouteSummary of Prefix * Regex.T * Regex.T
    | MaxAdvertisements of int


   
(*
let rec wf_loc r = 
    match r with 
    | Loc _ -> true
    | Inter(r1,r2) | Union(r1,r2) | Diff(r1,r2) ->
        (wf_loc r1) && (wf_loc r2)
    | _ -> false

let rec locs r =
    match r with 
    | Inter(r1,r2) -> Set.intersect (locs r1) (locs r2)
    | Union(r1,r2) -> Set.union (locs r1) (locs r2)
    | Diff(r1,r2) -> Set.difference (locs r1) (locs r2)
    | _ -> failwith "unreachable"
*)