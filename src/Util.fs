module Util

open System
open System.Collections.Generic

let unreachable() = failwith "unreachable"

[<Sealed>]
type Reindexer<'a when 'a : equality> = 
  class
    val mutable private Count : int
    val private IdToValue : Dictionary<int, 'a>
    val private ValueToId : Dictionary<'a, int>
    
    new(iec : IEqualityComparer<'a>) = 
      { Count = 0
        IdToValue = Dictionary()
        ValueToId = Dictionary(iec) }
    
    member x.Index(v) = 
      let b, i = x.ValueToId.TryGetValue(v)
      if b then i
      else 
        x.Count <- x.Count + 1
        x.IdToValue.[x.Count] <- v
        x.ValueToId.[v] <- x.Count
        x.Count
    
    member x.Value(i) = x.IdToValue.[i]
    member x.Iter(f) = x.ValueToId |> Seq.iter (fun kv -> f kv.Key kv.Value)
  end

module Debug = 
  let debug f = 
    let settings = Args.getSettings()
    if settings.Debug then f()
  
  let logInfo (idx, str) = 
    let settings = Args.getSettings()
    if settings.Debug then 
      let logFile = settings.DebugDir + "debug(" + string idx + ").log"
      System.IO.File.AppendAllText(logFile, str + "\n")

module Profile = 
  let time f x = 
    let s = System.Diagnostics.Stopwatch()
    s.Start()
    let ret = f x
    s.Stop()
    (ret, s.ElapsedMilliseconds)

module File = 
  let sep = string System.IO.Path.DirectorySeparatorChar
  
  let writeFileWithExtension dir ext text = 
    let file = dir + "." + ext
    System.IO.File.WriteAllText(file, text)
  
  let createDir path = 
    let dir = System.IO.Directory.CreateDirectory(path)
    dir.Create()

module List = 
  let inline fold f b ls = 
    let mutable acc = b
    for i in ls do
      acc <- f acc i
    acc
  
  let inline fold1 f ls = 
    match ls with
    | [] -> failwith "empty list in fold1"
    | hd :: tl -> 
      let mutable acc = hd
      for i in tl do
        acc <- f acc i
      acc
  
  let inline joinBy sep ss = fold1 (fun a b -> a + sep + b) ss
  
  let inline toString xs = 
    match xs with
    | [] -> "[]"
    | _ -> 
      let s = joinBy "," (List.map string xs)
      sprintf "[%s]" s
  
  let combinations n ls = 
    let rec aux acc size set = 
      seq { 
        match size, set with
        | n, x :: xs -> 
          if n > 0 then yield! aux (x :: acc) (n - 1) xs
          if n >= 0 then yield! aux acc n xs
        | 0, [] -> yield acc
        | _, [] -> ()
      }
    aux [] n ls

module Set = 
  let inline fold1 f xs = 
    if Set.isEmpty xs then failwith "empty set in fold1"
    else 
      let x = Set.minElement xs
      let xs' = Set.remove x xs
      Set.fold f x xs'
  
  let inline joinBy sep ss = fold1 (fun a b -> a + sep + b) ss
  
  let inline toString ss = 
    if Set.isEmpty ss then "{}"
    else 
      let s = joinBy "," (Set.map string ss)
      sprintf "{%s}" s

module Dictionary = 
  let inline fold f b (d : Dictionary<_, _>) = 
    let mutable acc = b
    for kv in d do
      acc <- f acc kv.Key kv.Value
    acc
  
  let inline map f (d : Dictionary<_, _>) = 
    let acc = Dictionary(d.Count)
    for kv in d do
      acc.[kv.Key] <- f kv.Key kv.Value
    acc
  
  let inline filter f (d : Dictionary<_, _>) = 
    let acc = Dictionary()
    for kv in d do
      if f kv.Key kv.Value then acc.[kv.Key] <- kv.Value
    acc

module HashSet = 
  let inline fold f b (h : HashSet<_>) = 
    let mutable acc = b
    for v in h do
      acc <- f acc v
    acc
  
  let inline map f (h : HashSet<_>) = 
    let acc = HashSet()
    for v in h do
      acc.Add(f v) |> ignore
    acc
  
  let inline filter f (h : HashSet<_>) = 
    let acc = HashSet()
    for v in h do
      if f v then acc.Add v |> ignore
    acc

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
      | None -> Map.add k v s) b a

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
    | Err _ -> failwith "unwrapping error result"
  
  let map f res = 
    match res with
    | Ok v -> Ok(f v)
    | Err e -> Err e

module Format = 
  let first = ref false
  let obj = new Object()
  let footerSize = 80
  let offset = 9
  
  let indent (i : int) (tabs : bool) (s : string) : string = 
    let ind = 
      if tabs then String.replicate i "\t"
      else String.replicate i " "
    
    let s = s.Replace("\n", "\n" + ind)
    (ind + s).TrimEnd(' ')
  
  let wrapText (s : string) : string = 
    let s = s.Trim()
    let words = s.Split(' ')
    let mutable count = offset
    let mutable result = ""
    for word in words do
      let len = word.Length
      if count + len + 2 > footerSize then 
        let spaces = String.replicate offset " "
        result <- result + "\n" + spaces + word
        count <- word.Length + offset
      else 
        let space = 
          (if result = "" then ""
           else " ")
        result <- result + space + word
        count <- count + len + 1
    result
  
  let writeColor (s : string) c = 
    Console.ForegroundColor <- c
    Console.Write s
    Console.ResetColor()
  
  let inline cyan (s : string) = lock obj (fun () -> writeColor s ConsoleColor.DarkCyan)
  let inline green (s : string) = lock obj (fun () -> writeColor s ConsoleColor.DarkGreen)
  let inline red (s : string) = lock obj (fun () -> writeColor s ConsoleColor.DarkRed)
  let inline gray (s : string) = lock obj (fun () -> writeColor s ConsoleColor.DarkGray)
  
  let writeFormatted (s : string) = 
    let arr = s.Split('#')
    if arr.Length <= 2 then printf "%s" s
    for s in arr.[0..(arr.Length - 1)] do
      if s.Length >= 6 && s.[0..4] = "(red)" then red s.[5..]
      elif s.Length >= 8 && s.[0..6] = "(green)" then green s.[7..]
      elif s.Length >= 7 && s.[0..5] = "(gray)" then gray s.[6..]
      elif s.Length >= 7 && s.[0..5] = "(cyan)" then cyan s.[6..]
      else lock obj (fun () -> printf "%s" s)
  
  let writeFooter() = 
    let banner = String.replicate footerSize "-"
    writeFormatted (sprintf "#(gray)%s#\n" banner)
  
  let writeHeader() = 
    let settings = Args.getSettings()
    
    let name = 
      match settings.PolFile with
      | Some f -> f
      | None -> "foo"
    
    let sep = System.IO.Path.DirectorySeparatorChar
    let arr = name.Split(sep)
    let len = arr.Length
    
    let name = 
      if len > 1 then arr.[len - 2] + (string sep) + arr.[len - 1]
      else arr.[len - 1]
    lock obj (fun () -> 
      if not !first then 
        first := true
        writeFooter()
      writeFormatted (sprintf "#(cyan)%s#\n" name))
  
  let error str = 
    lock obj (fun () -> 
      writeHeader()
      printfn ""
      let s = "Error:   "
      writeColor s ConsoleColor.DarkRed
      printfn "%s" (wrapText str)
      writeFooter()
      exit 0)
  
  let warning str = 
    lock obj (fun () -> 
      writeHeader()
      printfn ""
      let s = "Warning: "
      writeColor s ConsoleColor.DarkYellow
      printfn "%s" (wrapText str)
      writeFooter())
  
  let header s = 
    let eqs = "========="
    let right = eqs + "> "
    let left = " <" + eqs
    sprintf "#(cyan)%s%s%s#\n" right s left
  
  let passed() = writeFormatted "#(green)passed#\n"
  let failed() = writeFormatted "#(red)failed#\n"