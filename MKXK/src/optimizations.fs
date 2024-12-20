namespace MKXK.Optimizations
open System
open FSharp.NativeInterop
#nowarn "9"


type Population<'T when 'T:unmanaged>(n:int) =
    let population = Array.zeroCreate<'T> n
    let generation = Array.zeroCreate<int32> n
    let fitness    = Array.zeroCreate<float> n
    let mutable count = 0
    let mutable iteration = 0
    let len_t = System.Runtime.InteropServices.Marshal.SizeOf<'T>()    

    member x.Population = population
    member x.Generation = generation
    member x.Fitness = fitness
    member x.Capacity = n
    member x.TSize = len_t
    member x.Item
        with get (idx) = population[idx] 
        and set idx value = population[idx] <- value
    member x.Count with get() = count and set(value) = count <- min value n
    member x.Iteration with get() = iteration and set(value) = iteration <- value


type GeneticAlg<'T when 'T:unmanaged>(n:int, m:int) =
    let population   = Population<'T>(n)
    let matingpool_a = Population<'T>(m)
    let matingpool_b = Population<'T>(m)
    let mutable converged = false
    let mutable n_iteration = 0
    let mutable steps: list<Population<'T> -> unit> = []

    member x.Run() =
        while not converged do
            for step in steps do step population
            n_iteration <- n_iteration + 1


module Population =
    let init n (a: int -> 'T) =
        let p = Population<'T>(n)
        p.Count <- n
        for i in 0..p.Count - 1 do p.Population[i] <- a i
        p

    let iter (f:'T -> unit) (p:Population<'T>) =
        for i in 0..p.Count - 1 do f p[i]

    let iteri (f:int -> unit) (p:Population<'T>) =
        for i in 0..p.Count - 1 do f i 

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
        for i in 0..p.Count - 2 do
            for j in (i + 1)..p.Count - 1 do
                if p.Generation[j] > p.Generation[i] then swap i j p
        p
                    
                    
    let sortByGenerationDescending (p:Population<'T>) =
        for i in 0..p.Count - 2 do
            for j in (i + 1)..p.Count - 1 do
                if p.Generation[j] < p.Generation[i] then swap i j p
        p


    let sortByFitness (p:Population<'T>) =
        for i in 0..p.Count - 2 do
            for j in (i + 1)..p.Count - 1 do
                if p.Fitness[j] > p.Fitness[i] then swap i j p
        p


    let sortByFitnessDescending (p:Population<'T>) =
        for i in 0..p.Count - 2 do
            for j in (i + 1)..p.Count - 1 do
                if p.Fitness[j] < p.Fitness[i] then swap i j p
        p


    let constrain (cons:'T -> 'T) (p:Population<'T>) =
        for i in 0..p.Count - 1 do p.Population[i] <- cons p.Population[i]
        p

    let fitness (fit:'T -> float) (p:Population<'T>) =
        for i in 0..p.Count - 1 do p.Fitness[i] <- fit p.Population[i]
        p

    
// type selection

module Mutation =
    let private r = Random()

    /// define a custom function for mutating the population
    let mutate occ (mut:'T -> 'T) (p:Population<'T>) =
        for i in 0..p.Count - 1 do
            match r.NextDouble() with
            | d when d < occ -> p[i] <- mut p[i]
            | _ -> ()
        p
            
    
    let bitflip occ (p:Population<'T>) = 
        for i in 0..p.Count - 1 do
            match r.NextDouble() with
            | d when d < occ -> 
                use ptr = fixed &p.Population[i]
                let span = Span<byte>(NativePtr.toVoidPtr ptr, p.TSize)
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
                span[idx] <- span[idx] &&& mut
            | _ -> ()     
        p


    let random occ (p:Population<'T>) =
        for i in 0..p.Count - 1 do
            match r.NextDouble() with 
            | d when d < occ ->
                use ptr = fixed &p.Population[i]
                let span = Span<byte>(NativePtr.toVoidPtr ptr, p.TSize)
                r.NextBytes(span)
            | _ -> ()    
        p


    let scramble occ (p:Population<'T>) =
        for i in 0..p.Count - 1 do
            match r.NextDouble() with
            | d when d < occ ->
                use ptr = fixed &p.Population[i]
                let span = Span<byte>(NativePtr.toVoidPtr ptr, p.TSize)
                let idx = r.Next(p.TSize - 8)
                r.NextBytes(span.Slice(idx,8))
            | _ -> ()
        p
            

    let swap occ (p:Population<'T>) = 
        failwith "Mutation.swap is not implemented yet"


    let inversion occ (p:Population<'T>) =
        failwith "Mutation.inversion is not implemented yet"


module Crossover =
    let private r = Random()

    let inline private stackalloc<'a when 'a: unmanaged> (length: int): Span<'a> =
        let p = NativePtr.stackalloc<'a> length |> NativePtr.toVoidPtr
        Span<'a>(p, length)

    /// a: MatingPool, b: Population
    let crossover (f: 'T * 'T -> 'T * 'T) (a:Population<'T>) (b:Population<'T>) =
        let len = if a.Count % 2 = 0 then a.Count else a.Count - 1
        for i in 0..2..len - 1 do
            let pA = a[i + 0]
            let pB = a[i + 1]
            let (cA,cB) = f (pA,pB)
            b[i + 0] <- cA
            b[i + 1] <- cB
        b
            

    let onepoint (a:Population<'T>) (b:Population<'T>) =
        let len = min a.Count b.Count
        use a' = fixed &a.Population[0]
        use b' = fixed &b.Population[0]
        
        for i in 0..len - 1 do
            let ptr_a = NativePtr.add a' i |> NativePtr.toVoidPtr
            let ptr_b = NativePtr.add b' i |> NativePtr.toVoidPtr
            let span_a = Span<byte>(ptr_a, a.TSize)
            let span_b = Span<byte>(ptr_b, b.TSize)
            let idx = r.Next(a.TSize - 8)        
            let A = idx - 1
            let B = b.TSize - 1
            for i in 0..A do span_a[i] <- span_b[i]
            for j in A..B do span_b[j] <- span_a[j]
            

    let multipoint (conv:'T -> voidptr * int) (a:Population<'T>) (b:Population<'T>) =
        failwith "Crossover.multipoint is not implemented yet"
            

    let uniform (a:Population<'T>) (b:Population<'T>) =
        let len = min a.Count b.Count
        let size = a.TSize / 8
        use a' = fixed &a.Population[0]
        use b' = fixed &b.Population[0]

        for i in 0..len - 1 do
            let ptr_a = NativePtr.add a' i |> NativePtr.toVoidPtr
            let ptr_b = NativePtr.add b' i |> NativePtr.toVoidPtr
            let span_a = Span<float>(ptr_a, size)
            let span_b = Span<float>(ptr_b, size)

            for j in 0..size - 1 do    // offset 8-bytes stride
                match r.NextDouble() with
                | d when d < 0.5 ->
                    let ga = span_a[j]
                    let gb = span_b[j]                   
                    span_a[j] <- gb
                    span_b[j] <- ga
                | _ -> ()


    let arithmetic cf (a:Population<'T>) (b:Population<'T>) =
        let len = min a.Count b.Count
        let size = a.TSize / 8
        use a' = fixed &a.Population[0]
        use b' = fixed &b.Population[0]

        for i in 0..len - 1 do
            let ptr_a = NativePtr.add a' i |> NativePtr.toVoidPtr
            let ptr_b = NativePtr.add b' i |> NativePtr.toVoidPtr
            let span_a = Span<float>(ptr_a, size)
            let span_b = Span<float>(ptr_b, size)

            for j in 0..size - 1 do
                let ga = cf * span_a[j] + (1. - cf) * span_b[j]
                let gb = cf * span_b[j] * (1. - cf) * span_a[j]
                span_a[j] <- gb
                span_b[j] <- ga

    
module Selection =
    let private r = Random()

    /// b: MatingPool, a: OriginalPopulation
    let proportional (a:Population<'T>) (b:Population<'T>) =
        let len = min a.Count b.Capacity
        let mutable count = 0
        let indices = [|0..a.Count - 1|]
        r.Shuffle indices
        for i in 0..len - 1 do 
            b[i] <- a[indices[i]]
            count <- count + 1
        b.Count <- count
        b

    /// b: MatingPool, a: OriginalPopulation
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
        b.Count <- count
        b


    let stochasticUniversalSampling (a:Population<'T>) (b:Population<'T>) =
        failwith "Selection.stochasticUniversalSampling is not yet implemented"
        
    /// b: MatingPool, a: OriginalPopulation
    let tournament (a:Population<'T>) (b:Population<'T>) =
        let mutable count = 0
        let indices = [|0..a.Capacity - 1|]
        Random.Shared.Shuffle indices
        for i in 0..2..a.Capacity - 1 do
            let idx0 = indices[i + 0]
            let idx1 = indices[i + 1]
            if a.Fitness[idx0] > a.Fitness[idx1] then b[count] <- a[idx0] else b[count] <- a[idx1]
            count <- count + 1
        b.Count <- count
        b
            
    /// b: MatingPool, a: OriginalPopulation
    let rank (a:Population<'T>) (b:Population<'T>) =
         failwith "Selection.rank is not implemented yet"

    /// b: MatingPool, a: OriginalPopulation
    let random (a:Population<'T>) (b:Population<'T>) =
        let len = min a.Count b.Capacity
        let mutable count = 0
        for i in 0..a.Count - 1 do
            match r.NextDouble() with
            | d when d < 0.8 && count < b.Capacity -> 
                b[count] <- a[i]
                count <- count + 1
            | _ -> ()
        b.Count <- count
        b


module Termination =
    let onGeneration max_generation (p:Population<'T>) =
        Array.max p.Generation >= max_generation

    let onIteration max_iteration (p:Population<'T>) =
        p.Iteration >= max_iteration


