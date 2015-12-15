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
    | SetComm of int array * string
    | SetMed of int
    | SetLP of int
    | Originate

type Actions = Action list

type Rule =
    {Import: Match;
     Export: Actions}

type T = Map<string, Rule list>

/// Debug config output
val format: T -> string

/// Generate the BGP match/action rules that are guaranteed to 
/// implement the user policy under all possible failure scenarios. 
/// This function returns an intermediate representation (IR) for BGP policies
val compileToIR: Topology.T -> Regex.REBuilder -> Regex.T list -> string -> Result<T, CounterExample>