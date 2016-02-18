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

type Filter = 
    | Deny
    | Allow of Import * (Export list)

type DeviceConfig =
    {Originates: bool;
     Filters: Filter list}

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

/// An final, merged, configuration
type T = Map<string, RouterConfig>

/// Debug config output
val format: T -> string

/// Performance + size statistics from compilation
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

/// Result of aggregation safety failure analysis
/// Returns (number of failures, pfx loc, agg loc, prefix, aggregate) option
type AggregationSafetyResult = (int * string * string * Prefix.T * Prefix.T) option


/// Compile a policy
val compileAllPrefixes: string -> Topology.T -> Ast.PolicyPair list -> Ast.CConstraint list -> T * AggregationSafetyResult * Stats


/// Unit tests
module Test = 
    val run: unit -> unit