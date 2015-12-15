module IR
open CGraph
open Common.Error

type CounterExample = 
    | UnusedPreferences of Map<int, Regex.T>
    | NoPathForRouters of Set<string>
    | InconsistentPrefs of CgState * CgState

type Match = 
    | Peer of string 
    | State of int array * string
    | PathRE of Regex.T
    | NoMatch

type Action = 
    | NoAction
    | SetComm of int array
    | SetMed of int
    | PrependPath of int

type LocalPref = int
type Peer = string
type Import = Match * LocalPref
type Export = Peer * Action list

type DeviceConfig =
    {(* Prefix: Prefix.T; *)
     Originates: bool;
     Filters: (Import * Export list) list}

type T = Map<string, DeviceConfig>

/// Debug config output
val format: T -> string

/// Generate the BGP match/action rules that are guaranteed to 
/// implement the user policy under all possible failure scenarios. 
/// This function returns an intermediate representation (IR) for BGP policies
val compileToIR: Topology.T -> Regex.REBuilder -> Regex.T list -> string -> Result<T, CounterExample>