#load "../src/optimizations.fs"

open MKXK.Optimizations
open System
open System.Runtime.CompilerServices
open FSharp.NativeInterop

type [<Struct>] V = {x:int; y:int; z:int}

let vs = [|for i in 0..9 -> {x = i; y = i; z = i}|]
let p0 = Population.init 10 (fun i -> vs[i])
let pm = Population<V>(10)

let mut (v:V) :V = 
    let d = Random.Shared.Next(10)
    let a = if Random.Shared.NextDouble() < 0.5 then -1 else 1
    match Random.Shared.Next(3) with
    | 0 -> {v with x = v.x + a * d}
    | 1 -> {v with y = v.y + a * d}
    | 2 -> {v with z = v.z + a * d}
    | _ -> v

let con (v:V) :V =
    let x = Math.Clamp(v.x, 0, 10)
    let y = Math.Clamp(v.y, 2, 12)
    let z = Math.Clamp(v.z, 3, 12)
    {x = x; y = y; z = z}

let cross (ab: V * V) :V * V =
    let (a,b) = ab
    let (xa,xb) = if Random.Shared.NextDouble() < 0.5 then (a.x,b.x) else (b.x,a.x)
    let (ya,yb) = if Random.Shared.NextDouble() < 0.5 then (a.y,b.y) else (b.y,a.y)
    let (za,zb) = if Random.Shared.NextDouble() < 0.5 then (a.z,b.z) else (b.z,a.z)
    ({x=xa; y=ya; z=za},{x=xb; y=yb; z=zb})

p0
|> Population.fitness (fun v -> v.y)
|> Population.sortByFitness
|> Mutation.mutate 0.33 mut
|> Population.constrain con
|> Population.iter Console.WriteLine

printfn "\n##############\n"

pm
|> Selection.random p0

p0
|> Crossover.crossover cross pm
|> Population.iter Console.WriteLine
