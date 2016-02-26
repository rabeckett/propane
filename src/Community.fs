module Community

type Pred = 
    | Bot 
    | Val of Set<Set<string> * Set<string>>
    
    override this.ToString() =
        let aux (p,n) =
            let x = if Set.isEmpty p then "" else Util.Set.joinBy " and " p
            let y = if Set.isEmpty n then "" else Util.Set.joinBy " and " (Set.map (fun s -> "not " + s) n)
            let str = 
                match x, y with
                | "", "" -> "true"
                | _, "" -> x
                | "", _ -> y
                | _, _ -> x + " and " + y
            if Set.count p + Set.count n > 1 then "(" + str + ")" else str
        match this with
        | Bot -> "false"
        | Val cs ->
            match Set.count cs with 
            | 0 -> "true"
            | 1 -> aux (Set.minElement cs)
            | _ ->
                let strs = Set.map aux cs
                let all = Util.Set.joinBy " or " strs
                "(" + all + ")"

let bot = Bot 

let top = Val Set.empty

let value v = Val (Set.singleton (Set.singleton v, Set.empty))

let factor (a,b) (c,d) = 
    let allSame = (Set.union a b = Set.union c d)
    if allSame then
        Some (Set.intersect a c, Set.intersect b d)
    else None

let find2 xs = 
    let mutable found = None
    for x in xs do 
        for y in xs do 
            if x <> y then 
                match factor x y with
                | None -> ()
                | Some z -> found <- Some (x,y,z)
    found

let simplify xs =
    let mutable change = true
    let mutable xs = xs
    while change do
        change <- false
        match find2 xs with
        | None -> ()
        | Some (x,y,z) -> 
            change <- true
            xs <- Set.remove x xs 
            xs <- Set.remove y xs
            xs <- Set.add z xs
    xs

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
            else Val cross

let negate c1 = 
    match c1 with
    | Bot -> top
    | Val xs -> 
        if Set.isEmpty xs then bot
        else
            Set.map (fun (p,n) -> 
                Val (Set.union 
                    (Set.map (fun s -> (Set.singleton s, Set.empty)) n)
                    (Set.map (fun s -> (Set.empty, Set.singleton s)) p)) ) xs
            |> Set.fold conj top