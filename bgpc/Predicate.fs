module Predicate


type Pair = 
    {Prefix: Prefix.Pred;
     Comm: Community.Pred}

let botPair = 
    {Prefix = Prefix.bot; 
        Comm = Community.bot}

let topPair = 
    {Prefix = Prefix.top; 
     Comm = Community.top}

type T =
    | Pred of Set<Pair>

    override this.ToString() =
        let aux parens pair = 
            if pair = topPair then "true"
            else if pair = botPair then "false"
            else if pair.Comm = Community.top then Prefix.str pair.Prefix 
            else if pair.Prefix = Prefix.top then string pair.Comm
            else
                let ret = sprintf "%s and %s" (Prefix.str pair.Prefix) (string pair.Comm)
                if parens then "(" + ret + ")" else ret
        let (Pred pairs) = this 
        if Set.count pairs = 1 then
            aux false (Set.minElement pairs)
        else Common.Set.joinBy " or " (Set.map (aux true) pairs)

let bot = Pred (Set.singleton botPair)

let top = Pred (Set.singleton topPair)

let prefix p slash = 
    Pred (Set.singleton 
        {Prefix = Prefix.toPredicate [Prefix.prefix p slash]; 
         Comm = Community.top})

let community c = 
    Pred (Set.singleton
        {Prefix = Prefix.top;
         Comm = Community.value c})

let join x y = 
    {Prefix = Prefix.conj x.Prefix y.Prefix;
     Comm = Community.conj x.Comm y.Comm}

let factor x y = 
    let xc, yc = x.Comm, y.Comm
    let xp, yp = x.Prefix, y.Prefix
    match (xp = yp), (xc = yc) with
    | true, _ -> Some {Prefix=xp; Comm=Community.disj xc yc}
    | _, true -> Some {Prefix=Prefix.disj xp yp; Comm=xc}
    | false, false -> None

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
    xs <- Set.map (fun x -> if x.Prefix = Prefix.bot || x.Comm = Community.bot then botPair else x) xs
    if Set.isEmpty xs then xs
    else if Set.contains topPair xs then Set.singleton topPair 
    else Set.remove botPair xs

let disj x y = 
    let (Pred xs) = x
    let (Pred ys) = y
    Pred (simplify (Set.union xs ys))

let conj x y =
    let (Pred xs) = x
    let (Pred ys) = y
    let mutable cross = Set.empty 
    for p1 in xs do 
        for p2 in ys do
            let z = join p1 p2
            if z <> botPair then
                cross <- Set.add z cross
    Pred (simplify cross)
        
let negate x  =
    let (Pred xs) = x
    Set.map (fun x -> 
        let a = {Prefix = Prefix.negation x.Prefix; Comm = Community.top}
        let b = {Prefix = Prefix.top; Comm = Community.negate x.Comm}
        Pred (simplify (Set.ofList [a; b])) ) xs
    |> Common.Set.fold1 conj
