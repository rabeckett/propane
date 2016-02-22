module Abgp

type T

type Stats = 
    {NumPrefixes: int;
     ConfigSize: int;
     PrefixTime: int64;
     PerPrefixTimes: int64 array;
     PerPrefixBuildTimes: int64 array;
     PerPrefixMinTimes: int64 array;
     PerPrefixOrderTimes: int64 array;
     PerPrefixGenTimes: int64 array;
     JoinTime: int64;
     MinTime: int64}

type AggregationSafety = 
    {NumFailures: int; 
     PrefixLoc: string; 
     AggregateLoc: string; 
     Prefix: Prefix.T;
     Aggregate: Prefix.T}
    
type CompilationResult =
    {Abgp: T;
     AggSafety: AggregationSafety option;
     Stats: Stats}

val format: T -> string

val compileAllPrefixes: string -> Ast.PolInfo -> CompilationResult

module Test = 
    val run: unit -> unit