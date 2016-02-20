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
        match Set.count pairs with 
        | 0 -> "false"
        | 1 -> aux false (Set.minElement pairs)
        | _ -> Common.Set.joinBy " or " (Set.map (aux true) pairs)

let bot = Pred (Set.empty)

let top = Pred (Set.singleton topPair)

let prefix p slash = 
    Pred (Set.singleton 
        {Prefix = Prefix.toPredicate [Prefix.prefix p slash]; 
         Comm = Community.top})

let prefixPred x =
    Pred (Set.singleton 
        {Prefix = x; 
         Comm = Community.top})

let community c = 
    Pred (Set.singleton
        {Prefix = Prefix.top;
         Comm = Community.value c})

let communityPred x =
    Pred (Set.singleton
        {Prefix = Prefix.top;
         Comm = x})

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
    xs <- Set.filter (fun x -> x.Prefix <> Prefix.bot && x.Comm <> Community.bot) xs
    if Set.contains topPair xs then Set.singleton topPair 
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
    if x = bot then top else
    let (Pred xs) = x
    Set.map (fun x -> 
        let a = {Prefix = Prefix.negation x.Prefix; Comm = Community.top}
        let b = {Prefix = Prefix.top; Comm = Community.negate x.Comm}
        Pred (simplify (Set.ofList [a; b])) ) xs
    |> Common.Set.fold1 conj

let implies x y = 
    disj (negate x) y = top

let getPrefixes (Pred x:T) = 
    Seq.map (fun pair -> pair.Prefix) x

let example x = 
    let (Pred xs) = x
    if Set.isEmpty xs then "false" else
    let a = Set.minElement xs
    let exPrefix = 
        match Prefix.toPrefixes a.Prefix with
        | [] -> "false"
        | x::_ -> string x
    let exComm = 
        match a.Comm with
        | Community.Bot -> "false"
        | Community.Val xs -> 
            if Set.isEmpty xs then "true" else
            Set.minElement xs 
            |> Set.singleton
            |> Community.Val
            |> string
    match exPrefix, exComm with
    | "false", _ -> "false"
    | _, "false" -> "false"
    | _, "true" -> exPrefix
    | "0.0.0.0/0", _ -> exComm
    | _, _ -> exPrefix + " and " + exComm


module Test = 

    open Common.Format

    let maxTests = 1000

    let rand = System.Random()

    let randomPrefix () =
        let lo = uint32 (rand.Next())
        let hi = uint32 (rand.Next()) + lo
        Prefix.fromRange (lo,hi)

    let testPrefixes () =
        printf "Prefix predicates "
        let mutable fail = false
        for i = 1 to maxTests do 
            let x = randomPrefix ()
            let y = Prefix.toPrefixes x
            let z = Prefix.toPredicate y
            let a = Prefix.str x 
            let b = Prefix.str z
            if a <> b then
                fail <- true
        if fail then failed () else passed ()

    let run () = 
        testPrefixes ()