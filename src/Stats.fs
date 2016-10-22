module Stats

let inline total xs = 
   xs
   |> Array.map float
   |> Array.sum

let inline totalInSec x = total x / 1000.0
let inline valueInSec x = float x / 1000.0

let printCsv (abgp : Abgp.Stats) (gen : Generate.Stats option) (parseTime : int64) (genTime : int64) = 
   printfn "%s" 
      ("Parse Topology+Policy, Total Compile to ABGP, Total Abgp to Low-level, " 
       + "PG Construction, PG Minimization, Aggregate Analysis, Find Preferences, Generate ABGP, " 
       + "ABGP Minimization, Generate Core, Generate Low-level, Substitution, Generate Vendor")
   let (genCore, genLowLevel, substitution, genVendor) = 
      match gen with
      | None -> (float 0, float 0, float 0, float 0)
      | Some stats -> 
         (valueInSec stats.CoreTime, valueInSec stats.GenLowLevelConfigTime, 
          valueInSec stats.SubstitutionTime, valueInSec stats.QuaggaTime)
   
   let parse = valueInSec parseTime
   let toAbgp = valueInSec abgp.PrefixTime
   let toConfig = valueInSec genTime
   let pgConstruction = totalInSec abgp.PerPrefixBuildTimes
   let pgMinimize = totalInSec abgp.PerPrefixMinTimes
   let aggAnalysis = totalInSec abgp.PerPrefixAggAnalysisTimes
   let findOrdering = totalInSec abgp.PerPrefixOrderTimes
   let genAbgp = totalInSec abgp.PerPrefixGenTimes
   let minAbgp = valueInSec abgp.MinTime
   printfn "%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f" parse toAbgp toConfig pgConstruction pgMinimize 
      aggAnalysis findOrdering genAbgp minAbgp genCore genLowLevel substitution genVendor

let print (abgp : Abgp.Stats) (gen : Generate.Stats option) (parseTime : int64) (genTime : int64) = 
   let (genCore, genLowLevel, substitution, genVendor) = 
      match gen with
      | None -> (float 0, float 0, float 0, float 0)
      | Some stats -> 
         (valueInSec stats.CoreTime, valueInSec stats.GenLowLevelConfigTime, 
          valueInSec stats.SubstitutionTime, valueInSec stats.QuaggaTime)
   
   let parse = valueInSec parseTime
   let toAbgp = valueInSec abgp.PrefixTime
   let toConfig = valueInSec genTime
   let pgConstruction = totalInSec abgp.PerPrefixBuildTimes
   let pgMinimize = totalInSec abgp.PerPrefixMinTimes
   let aggAnalysis = totalInSec abgp.PerPrefixAggAnalysisTimes
   let findOrdering = totalInSec abgp.PerPrefixOrderTimes
   let genAbgp = totalInSec abgp.PerPrefixGenTimes
   let minAbgp = valueInSec abgp.MinTime
   printfn "======== Compilation Statistics Summary ========"
   printfn "Total Parse Time:                   %f sec" parse
   printfn "Total Compile to ABGP:              %f sec" toAbgp
   printfn "Total Abgp to Device Configs:       %f sec" toConfig
   printfn "------------------------------------------------"
   printfn "PGIR Construction:                  %f sec" pgConstruction
   printfn "PGIR Minimization:                  %f sec" pgMinimize
   printfn "Aggregation Safety:                 %f sec" aggAnalysis
   printfn "Local-pref search:                  %f sec" findOrdering
   printfn "PGIR to ABGP:                       %f sec" genAbgp
   printfn "Minimize ABGP:                      %f sec" minAbgp
   printfn "Generate CORE:                      %f sec" genCore
   printfn "Generate Low-level:                 %f sec" genLowLevel
   printfn "Template Substitution:              %f sec" substitution
   printfn "Generate Vendor-specific:           %f sec" genVendor
   printfn "================================================"