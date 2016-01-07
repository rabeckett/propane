module IR
open CGraph
open Common.Error

type CounterExample = 
    | UnusedPreferences of Map<int, Regex.T>
    | NoPathForRouters of Set<string>
    | InconsistentPrefs of CgState * CgState
    | UncontrollableEnter of string
    | UncontrollablePeerPreference of string

type Match = 
    | Peer of string 
    | State of string * string
    | PathRE of Regex.T
    | NoMatch

type Action = 
    | NoAction
    | SetComm of string
    | SetMed of int
    | PrependPath of int

type LocalPref = int
type Peer = string
type Import = Match * LocalPref
type Export = Peer * Action list

type DeviceConfig =
    {Originates: bool;
     Filters: (Import * Export list) list}

type PolicyPair = Prefix.T list * Regex.REBuilder * Regex.T list

type PrefixConfig = Prefix.T list * Map<string, DeviceConfig>

type T = Map<string, (Prefix.T list * DeviceConfig) list>

/// Convert per-prefix representation to a per-router representatoin
val joinConfigs: PrefixConfig list -> T

/// Debug config output
val format: T -> string

/// Generate the BGP match/action rules that are guaranteed to 
/// implement the user policy under all possible failure scenarios for a given prefix. 
/// This function returns either an intermediate representation (IR) 
/// for BGP policies, or a counterexample indicating why compilation will not work.
val compileToIR: string -> int -> Prefix.T list -> Regex.REBuilder -> Regex.T list -> Result<PrefixConfig, CounterExample>

/// Compile to an intermediate representation for a given prefix. 
/// Gives a counterexample and quits the program if compilation is not possible.
val compileForSinglePrefix: string -> int -> PolicyPair -> PrefixConfig

/// Compile for all prefixes
val compileAllPrefixes: string -> PolicyPair list -> T