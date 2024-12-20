namespace MKXK.Optimizations
open System


type Population<'T>(n:int) =
    let population = Array.zeroCreate<'T> n
    let generation = Array.zeroCreate<int32> n
    let fitness    = Array.zeroCreate<float> n
    let mutable count = 0
    let mutable iteration = 0

    member x.Population = population
    member x.Generation = generation
    member x.Fitness = fitness
    member x.Capacity = n
    member x.Item
        with get (idx) = population[idx] 
        and set idx value = population[idx] <- value
    member x.Count with get() = count and set(value) = count <- min value n
    member x.Iteration with get() = iteration and set(value) = iteration <- value


type GeneticAlg<'T>(n:int, m:int) =
    let population   = Population(n)
    let matingpool_a = Population(m)
    let matingpool_b = Population(m)
    let mutable converged = false
    let mutable n_iteration = 0
    let mutable steps: list<Population<'T> -> unit> = []

    member x.Run() =
        while not converged do
            for step in steps do step population
            n_iteration <- n_iteration + 1


module Population =
    let init n (a: 'V -> 'T) (v:'V) =
        let p = Population<'T>(n)
        for i in 0..p.Capacity - 1 do p.Population[i] <- a v
        p

    let inline private swap i j (p:Population<'T>) =
        let g0 = p.Generation[i]
        let g1 = p.Generation[j] 
        let i0 = p.Population[i]
        let i1 = p.Population[j]
        let f0 = p.Fitness[i]
        let f1 = p.Fitness[j]

        p.Generation[i] <- g1
        p.Generation[j] <- g0
        p.Population[i] <- i1
        p.Population[j] <- i0        
        p.Fitness[i] <- f1
        p.Fitness[j] <- f0        


    let sortByGeneration (p:Population<'T>) =
        for i in 0..p.Capacity - 2 do
            for j in (i + 1)..p.Capacity - 1 do
                if p.Generation[j] > p.Generation[i] then swap i j p
                    
                    
    let sortByGenerationDescending (p:Population<'T>) =
        for i in 0..p.Capacity - 2 do
            for j in (i + 1)..p.Capacity - 1 do
                if p.Generation[j] < p.Generation[i] then swap i j p


    let sortByFitness (p:Population<'T>) =
        for i in 0..p.Capacity - 2 do
            for j in (i + 1)..p.Capacity - 1 do
                if p.Fitness[j] > p.Fitness[i] then swap i j p


    let sortByFitnessDescending (p:Population<'T>) =
        for i in 0..p.Capacity - 2 do
            for j in (i + 1)..p.Capacity - 1 do
                if p.Fitness[j] < p.Fitness[i] then swap i j p


    let constrain (cons:'T -> 'T) (p:Population<'T>) =
        for i in 0..p.Capacity - 1 do p.Population[i] <- cons p.Population[i]

    let fitness (fit:'T -> float) (p:Population<'T>) =
        for i in 0..p.Capacity - 1 do p.Fitness[i] <- fit p.Population[i]

    
// type selection

module Mutation =
    open System.Runtime.InteropServices
    open System.Runtime.CompilerServices
    open FSharp.NativeInterop

    let private r = Random()
    
    let bitflip occ (conv: 'T -> voidptr * int) (p:Population<'T>) = 
        for i in 0..p.Capacity - 1 do
            match r.NextDouble() with
            | d when d < occ -> 
                let ptr,len = conv p[i]
                let span = Span<float>(ptr, len)
                let mut =         
                    match r.Next(7) with
                    | 0 -> 0b000_0001
                    | 1 -> 0b000_0010 
                    | 2 -> 0b000_0100 
                    | 3 -> 0b000_1000 
                    | 4 -> 0b001_0000 
                    | 5 -> 0b010_0000 
                    | 6 -> 0b100_0000 
                    | _ -> 0b000_0000
                    |> byte
                let idx = r.Next(span.Length)
                let span = MemoryMarshal.AsBytes span
                span[idx] <- span[idx] &&& mut
            | _ -> ()     


    let random occ (conv: 'T -> 'T) (p:Population<'T>) =
        for i in 0..p.Capacity - 1 do
            match r.NextDouble() with 
            | d when d < occ -> p[i] <- conv p[i]
            | _ -> ()    


    let scramble occ (conv:array<'T> * int -> struct(nativeint * int)) (p:Population<'T>) =
        for i in 0..p.Capacity - 1 do
            match r.NextDouble() with
            | d when d < occ ->
                let struct(ptr, len) = conv(p.Population, i)
                let span = Span(ptr.ToPointer(), len)
                let mutable mut = r.NextInt64()
                let idx = r.Next(span.Length - 4)
                let span0 = MemoryMarshal.AsBytes span
                let span1 = Span<byte>(Unsafe.AsPointer(&mut), Marshal.SizeOf<int64>())
                span1.CopyTo(span0.Slice(idx))
            | _ -> ()
            

    let swap occ (p:Population<'T>) = 
        failwith "Mutation.swap is not implemented yet"


    let inversion occ (p:Population<'T>) =
        failwith "Mutation.inversion is not implemented yet"


module Crossover =
    let private r = Random()

    let onepoint (conv:'T -> voidptr * int) (a:Population<'T>) (b:Population<'T>) =
        let len = min a.Capacity b.Capacity
        for i in 0..len - 1 do
            let ptr_a,len_a = conv a[i]
            let ptr_b,len_b = conv b[i]
            let span_a = Span(ptr_a, len_a)
            let span_b = Span(ptr_b, len_b)
            let idx = r.Next(span_b.Length - 1)        
            let A = idx - 1
            let B = span_a.Length - 1
            for i in 0..A do span_a[i] <- span_b[i]
            for j in A..B do span_b[j] <- span_a[j]
            

    let multipoint (conv:'T -> voidptr * int) (a:Population<'T>) (b:Population<'T>) =
        failwith "Crossover.multipoint is not implemented yet"
            

    let uniform occ (conv:'T -> voidptr * int) (a:Population<'T>) (b:Population<'T>) =
        let len = min a.Capacity b.Capacity
        for i in 0..len - 1 do
            let ptr_a,len_a = conv a[i]
            let ptr_b,len_b = conv b[i]
            let span_a = Span(ptr_a, len_a)
            let span_b = Span(ptr_b, len_b)
            let span_len = min span_a.Length span_b.Length
            for j in 0..span_len - 1 do
                match r.NextDouble() with
                | d when d < occ ->
                    let ga = span_a[j]
                    let gb = span_b[j]
                    span_a[j] <- gb
                    span_b[j] <- ga
                | _ -> ()


    let arithmetic cf (conv:'T -> voidptr * int) (a:Population<'T>) (b:Population<'T>) =
        let len = min a.Capacity b.Capacity
        for i in 0..len - 1 do
            let ptr_a,len_a = conv a[i]
            let ptr_b,len_b = conv b[i]
            let span_a = Span(ptr_a, len_a)
            let span_b = Span(ptr_b, len_b)
            let span_len = min span_a.Length span_b.Length
            for j in 0..span_len - 1 do
                let ga = cf * span_a[j] + (1. - cf) * span_b[j]
                let gb = cf * span_b[j] * (1. - cf) * span_a[j]
                span_a[j] <- gb
                span_b[j] <- ga

    
module Selection =
    let private r = Random()

    let proportional (a:Population<'T>) (b:Population<'T>) =
        let indices = [|0..a.Capacity - 1|]
        Random.Shared.Shuffle indices
        let indices = indices[0..indices.Length / 2]
        for i in 0..indices.Length - 1 do b[i] <- a[indices[i]]


    let roulettewheel (a:Population<'T>) (b:Population<'T>) =
        let indices = ResizeArray []
        let f_min = Array.min a.Fitness
        let f_max = Array.max a.Fitness
        let mutable count = 0
        while count < b.Capacity do
            for i in 0..a.Capacity - 1 do
                let d = (a.Fitness[i] - f_min) / (f_max - f_min)
                if d < r.NextDouble() && not (indices.Contains i) && count < b.Capacity then
                    indices.Add i                    
                    count <- count + 1
        for i in 0..count - 1 do
            b[i] <- a[indices[i]]


    let stochasticUniversalSampling (a:Population<'T>) (b:Population<'T>) =
        failwith "Selection.stochasticUniversalSampling is not yet implemented"
        

    let tournament (a:Population<'T>) (b:Population<'T>) =
        let mutable n = 0
        let indices = [|0..a.Capacity - 1|]
        Random.Shared.Shuffle indices
        for i in 0..2..a.Capacity - 1 do
            let idx0 = indices[i + 0]
            let idx1 = indices[i + 1]
            if a.Fitness[idx0] > a.Fitness[idx1] then b[n] <- a[idx0] else b[n] <- a[idx1]
            n <- n + 1
            

    let rank (a:Population<'T>) (b:Population<'T>) =
         failwith "Selection.rank is not implemented yet"


    let random (a:Population<'T>) (b:Population<'T>) =
        let len = a.Capacity / 2
        let indices = Array.randomSample len [|0..a.Capacity|]
        for i in 0..indices.Length - 1 do b[i] <- a[indices[i]]        


module Termination =
    let onGeneration max_generation (p:Population<'T>) =
        Array.max p.Generation >= max_generation

    let onIteration max_iteration (p:Population<'T>) =
        p.Iteration >= max_iteration


