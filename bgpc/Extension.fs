module Extension

module List = 

    /// A faster, inline version of fold
    let inline fold f b ls = 
        let mutable acc = b 
        for i in ls do 
            acc <- f acc i 
        acc

    /// List fold without a required base element
    let inline fold1 f ls = 
        match ls with 
        | [] -> failwith "empty list in fold1"
        | hd::tl -> 
            let mutable acc = hd 
            for i in tl do 
                acc <- f acc i 
            acc

    /// Easier custom printing by joining a list of strings with a separator
    let inline joinBy sep ss = 
        fold1 (fun a b -> a + sep + b) ss

    /// Enumerate all length-n combinations of a list
    let combinations n ls = 
        let rec aux acc size set = seq {
            match size, set with 
            | n, x::xs -> 
                if n > 0 then yield! aux (x::acc) (n - 1) xs
                if n >= 0 then yield! aux acc n xs 
            | 0, [] -> yield acc 
            | _, [] -> () }
        aux [] n ls


module Error =

    type Result<'a, 'b> = 
        | Ok of 'a
        | Err of 'b

    let isOk res = 
        match res with 
        | Ok _ -> true 
        | Err _ -> false 

    let isErr res =
        match res with 
        | Ok _ -> false 
        | Err _ -> true

    let unwrap res = 
        match res with 
        | Ok v -> v 
        | Err _ -> 
            failwith "unwrapping error result"

    let map f res = 
        match res with 
        | Ok v -> Ok (f v)
        | Err e -> Err e

    let andThen f res = 
        match res with 
        | Ok v -> f v 
        | Err e -> Err e
