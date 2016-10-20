module Stats

let inline total xs = 
   xs
   |> Array.map float
   |> Array.sum

let inline totalInSec x = total x / 1000.0
let inline valueInSec x = float x / 1000.0

let print (abgp : Abgp.Stats) (gen : Generate.Stats option) (parseTime : int64) (genTime : int64) = 
   printfn "%s" 
      ("Build Topology+Policy, Total Compile to ABGP, Total Abgp to Low-level, " 
       + "PG Construction, PG Minimization, Find Preferences, Generate ABGP, " 
       + "ABGP Minimization, Generate Core, Generate Low-level, Substitution, Generate Quagga")
   let (w, x, y, z) = 
      match gen with
      | None -> (float 0, float 0, float 0, float 0)
      | Some stats -> 
         (valueInSec stats.CoreTime, valueInSec stats.GenLowLevelConfigTime, 
          valueInSec stats.SubstitutionTime, valueInSec stats.QuaggaTime)
   printfn "%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f" (valueInSec parseTime) (valueInSec abgp.PrefixTime) 
      (valueInSec genTime) (totalInSec abgp.PerPrefixBuildTimes) (totalInSec abgp.PerPrefixMinTimes) 
      (totalInSec abgp.PerPrefixOrderTimes) (totalInSec abgp.PerPrefixGenTimes) 
      (valueInSec abgp.MinTime) w x y z