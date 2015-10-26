module Config

type Match = 
    | Peer of string 
    | State of int array * string
    | PathRE of Regex.T

type Action = 
    | NoAction
    | SetComm of int array * string
    | SetMed of int
    | SetLP of int

type Actions = Action list

type Rule =
    {Import: Match;
     Export: Actions}

type T = Map<string, Rule list>

(* Generate templates 
let generateTemplates (config: T) (path: string) = 
    failwith "todo" *)
