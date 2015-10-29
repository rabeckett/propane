module Extension


module List = 

    let inline fold f b ls = 
        let mutable acc = b 
        for i in ls do 
            acc <- f acc i 
        acc

    let inline fold1 f ls = 
        match ls with 
        | [] -> failwith "empty list in fold1"
        | hd::tl -> 
            let mutable acc = hd 
            for i in tl do 
                acc <- f acc i 
            acc

    let inline joinBy sep ss = 
        fold1 (fun a b -> a + sep + b) ss


module Map = 

    let inline modify k d f map = 
        match Map.tryFind k map with 
        | None -> Map.add k d map
        | Some v -> Map.add k (f v) map


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
