open System

let J = 6
let value_m = Array.init J (fun i -> 0uy)
let value_p = Array.init J (fun i -> 1uy)
let values_all = Array.allPairs value_m value_p

// for i in 0..J..values_all.Length - 1 do
//     for j in 0..J - 1 do
//         printf "%d " values_all[i + j]

    
let values = Array.init (J * J) (fun i -> i)
for i in 0..values.Length - 1 do
    printfn "%B " values[i]
    // for j in 0..J - 1 do
