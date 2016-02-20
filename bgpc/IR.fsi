module IR


[<AutoOpen>]
module Types =

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

    type Filters =
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
        {Actions: (Predicate.T * Filters) list;
         Control: DeviceControl}

    type T = 
        {PolInfo: Ast.PolInfo; 
         RConfigs: Map<string, RouterConfig>}


module Display =
    val format: T -> string


module Compilation = 

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
         JoinTime: int64;
         MinTime: int64}

    type AggregationSafetyResult = (int * string * string * Prefix.T * Prefix.T) option

    val compileAllPrefixes: string -> Ast.PolInfo -> Ast.CConstraint list -> T * AggregationSafetyResult * Stats

   
module Test = 
    val run: unit -> unit