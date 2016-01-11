module Community


type Pred = 
    | Bot
    | Val of Set<Set<string> * Set<string>>
    
    override this.ToString() =
        let aux (p,n) =
            let x = if Set.isEmpty p then "" else Common.Set.joinBy " and " p
            let y = if Set.isEmpty n then "" else Common.Set.joinBy " and " (Set.map (fun s -> "!" + s) n)
            match x, y with
            | "", "" -> "true"
            | _, "" ->  "(" + x + ")"
            | "", _ -> "(" + y + ")"
            | _, _ -> "(" + x + " and " + y + ")"
        match this with
        | Bot -> "false"
        | Val cs -> 
            if Set.isEmpty cs then "true"
            else 
                let strs = Set.map aux cs
                Common.Set.joinBy " or " strs

let bot = Bot 

let top = Val Set.empty

let value v = Val (Set.singleton (Set.singleton v, Set.empty))

(* TODO *)
let simplify xs = xs

let disj c1 c2 =
    match c1, c2 with
    | Bot, _ -> c2
    | _, Bot -> c1
    | Val xs, Val ys ->
        Val (simplify (Set.union xs ys))

let conj c1 c2 = 
    match c1, c2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Val xs, Val ys ->
        printfn "xs: %A" xs
        printfn "ys: %A" ys
        if Set.isEmpty xs then c2 
        else if Set.isEmpty ys then c1
        else
            let mutable cross = Set.empty 
            for (xp, xn) in xs do 
                for (yp, yn) in ys do
                    let newP = Set.union xp yp 
                    let newN = Set.union xn yn
                    if Set.isEmpty (Set.intersect newP newN) then 
                        cross <- Set.add (newP, newN) cross
            let cross = simplify cross
            if Set.isEmpty cross then Bot 
            else Val (simplify cross)

let negate c1 = 
    match c1 with
    | Bot -> top
    | Val xs -> 
        if Set.isEmpty xs then bot
        else
            let clauses = 
                Set.map (fun (p,n) -> 
                    Val (Set.union 
                        (Set.map (fun s -> (Set.singleton s, Set.empty)) n)
                        (Set.map (fun s -> (Set.empty, Set.singleton s)) p))
                ) xs
            printfn "Clauses: %A" clauses
            let ret = Set.fold conj top clauses
            printfn "Negated: %A" ret
            ret