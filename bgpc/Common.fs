module Common

open System

module Debug =

    let debug n f =
        let settings = Args.getSettings ()
        if settings.Debug >= n then
            f ()

    let debug1 f = debug 1 f
    let debug2 f = debug 2 f
    let debug3 f = debug 3 f

    let logInfo n idx str =
        let settings = Args.getSettings ()
        let logFile = settings.DebugDir + "debug(" + string idx + ").log"
        let indent = String.replicate (n-1) "\t"
        if settings.Debug >= n then
            System.IO.File.AppendAllText(logFile, indent + str + "\n")

    let logInfo1(idx, f) = logInfo 1 idx f
    let logInfo2(idx, f) = logInfo 2 idx f
    let logInfo3(idx, f) = logInfo 3 idx f


module Profile =

    let time f x = 
        let s = System.Diagnostics.Stopwatch()
        s.Start()
        let ret = f x
        s.Stop()
        (ret, s.ElapsedMilliseconds)


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

    let inline toString xs = 
        match xs with 
        | [] -> "[]"
        | _ -> 
            let s = joinBy "," (List.map string xs)
            sprintf "[%s]" s

    let combinations n ls = 
        let rec aux acc size set = seq {
            match size, set with 
            | n, x::xs -> 
                if n > 0 then yield! aux (x::acc) (n - 1) xs
                if n >= 0 then yield! aux acc n xs 
            | 0, [] -> yield acc 
            | _, [] -> () }
        aux [] n ls


module Set = 

    let inline fold1 f xs = 
        if Set.isEmpty xs then 
            failwith "empty set in fold1"
        else 
            let x = Set.minElement xs
            let xs' = Set.remove x xs
            Set.fold f x xs'

    let inline joinBy sep ss =
        fold1 (fun a b -> a + sep + b) ss

    let inline toString ss = 
        if Set.isEmpty ss then "{}"
        else 
            let s = joinBy "," (Set.map string ss)
            sprintf "{%s}" s


module Option =

    let inline getOrDefault d o = 
        match o with
        | None -> d
        | Some x -> x


module Map = 

    let inline getOrDefault k d m = 
        match Map.tryFind k m with 
        | None -> d 
        | Some x -> x

    let inline adjust k d f m = 
        let current = getOrDefault k d m
        Map.add k (f current) m

    let merge a b f =
        Map.fold (fun s k v ->
            match Map.tryFind k s with
            | Some v' -> Map.add k (f k (v, v')) s
            | None -> Map.add k v s) a b


module Error =

    let exit () = exit 0

    let obj = new Object()

    let writeColor (s: string) c = 
        Console.ForegroundColor <- c
        Console.Write s
        Console.ResetColor ()

    let error (s: string) = 
        lock obj (fun () ->  
            writeColor "[Error]:" ConsoleColor.DarkRed
            Console.WriteLine s)
        exit ()

    let warning (s: string) =
        lock obj (fun () -> 
            writeColor "[Warning]: " ConsoleColor.DarkYellow
            Console.WriteLine s)

    let parseError (s: string) =
        lock obj (fun () -> 
            writeColor "[Parse Error]: " ConsoleColor.DarkRed
            Console.WriteLine s)
        exit ()

    let unimplementable (s: string) =
        lock obj (fun () ->
            writeColor "[Unimplementable]: " ConsoleColor.DarkRed
            Console.WriteLine s)
        exit ()

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
        | Err _ -> failwith "unwrapping error result"

    let map f res = 
        match res with 
        | Ok v -> Ok (f v)
        | Err e -> Err e

