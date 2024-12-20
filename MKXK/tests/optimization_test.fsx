#load "../src/optimizations.fs"

open MKXK.Optimizations
open System
open System.Runtime.CompilerServices
open FSharp.NativeInterop

type [<Struct>] V = {x:int; y:int; z:int}

let vs = [|for i in 0..10 -> string i, {x = i; y = i; z = i}|]
let p0 = Population<string * V> (10)
for i in 0..p0.Capacity - 1 do p0.Population[i] <- vs[i]

p0 |> Population.fitness (fun (s,v) -> float v.y)
Population.sortByFitness p0
p0.Fitness |> Array.iter Console.WriteLine

let func (vec:array<string * V>, i:int) =
    let (s,v) = vec[i]
    use ptr = fixed vec 
    let ptr' = NativePtr.toNativeInt (snd ptr)
    struct(ptr',12)
    
// func p0[0]
Mutation.scramble 0.3 func p0
p0.Population |> Array.iter (fun x -> printfn "%A" x)
