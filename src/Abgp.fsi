module Abgp

/// Abstract ABGP type T
type T

/// Compilation statistics for the polilcy
type Stats = 
   { NumPrefixes : int
     ConfigSize : int
     PrefixTime : int64
     PerPrefixTimes : int64 array
     PerPrefixBuildTimes : int64 array
     PerPrefixMinTimes : int64 array
     PerPrefixAggAnalysisTimes : int64 array
     PerPrefixOrderTimes : int64 array
     PerPrefixInboundTimes : int64 array
     PerPrefixGenTimes : int64 array
     JoinTime : int64
     MinTime : int64 }

/// Safety information related to aggregation
type AggregationSafety = 
   { NumFailures : int
     PrefixLoc : string
     AggregateLoc : string
     Prefix : Route.Prefix
     Aggregate : Route.Prefix }

/// Result from compilation, including:
/// (1) The final, compiled policy
/// (2) Aggregation safety information
/// (3) Compilation statistics
type CompilationResult = 
   { Abgp : T
     AggSafety : AggregationSafety option
     Stats : Stats }

/// Display the ABGP policy in a readable format
val format : T -> string
/// Compile the entire policy for all prefixes
val compileAllPrefixes : Ast.PolInfo -> CompilationResult
/// Convert the ABGP configuration to a more concrete configuration.
val toConfig : T -> Config.NetworkConfiguration
/// Display the Abgp policy in a CBGP manner for a given router
val getCBGPConfig : T -> CGraph.CgState -> string

module Test = 
   val run : unit -> unit