module IR


type Match = 
    | Peer of string 
    | State of string * string
    | PathRE of Regex.T
    | NoMatch

type Action = 
    | SetComm of string
    | SetMed of int
    | PrependPath of int

type LocalPref = int
type Peer = string
type Import = Match * LocalPref
type Export = Peer * Action list

/// Result from compiling a single prefix

type Filter = 
    | Deny
    | Allow of Import * (Export list)

type DeviceConfig =
    {Originates: bool;
     Filters: Filter list}

type AggregationSafetyResult = (int * string * string * Prefix.T * Prefix.T) option


/// Result from compiling the entire policy

type DeviceAggregates = (Prefix.T * seq<string>) list
type DeviceTags = ((string * Prefix.T list) * seq<string>) list
type DeviceMaxRoutes = (uint32 * seq<string>) list

type DeviceControl = 
    {Aggregates: DeviceAggregates;
     Tags: DeviceTags;
     MaxRoutes: DeviceMaxRoutes}

type RouterConfig = 
    {Actions: (Predicate.T * DeviceConfig) list;
     Control: DeviceControl}

type T = Map<string, RouterConfig>


/// Debug config output
val format: T -> string

type Stats = 
    {NumPrefixes: int;
     SizeRaw: int;
     SizeCompressed: int;
     TotalTime: int64;
     PrefixTime: int64;
     PerPrefixTimes: int64 array;
     PerPrefixBuildTimes: int64 array;
     PerPrefixMinTimes: int64 array;
     PerPrefixOrderTimes: int64 array;
     PerPrefixGenTimes: int64 array;
     JoinTime: int64;}

/// Compile for all prefixes
val compileAllPrefixes: string -> Topology.T -> Ast.PolicyPair list -> Ast.CConstraint list -> T * AggregationSafetyResult * Stats


/// Compilation unit tests
module Test = 
    val run: unit -> unit