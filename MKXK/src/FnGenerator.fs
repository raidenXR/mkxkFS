namespace MKXK
open System
open System.Collections.Generic
open ExprTree

type FnDescription = {
    fn: Expr
    latex: string
    terms: int
    count: int
    err: float
}

type TokenIdProbs = {
        Pnumber: int
        Pconstant: int
        Pvariable: int
        Pfunction: int
        Pbinary: int
        Punary: int
        Pparen: int
        Pfrac: int
        Psum: int
        Pprod: int
        Pint: int
        Pdiff: int
    } 
    with 
        static member Default = {
            Pnumber = 10
            Pconstant = 10
            Pvariable = 20
            Pfunction = 10
            Pbinary = 25
            Punary = 15
            Pparen = 10
            Pfrac = 10
            Psum = 0
            Pprod = 0
            Pint = 0
            Pdiff = 0
        }
        member x.ToArray() = Array.map float [|
            x.Pnumber; x.Pconstant; x.Pvariable; x.Pfunction
            x.Pbinary; x.Punary; x.Pparen; x.Pfrac
            x.Psum; x.Pprod; x.Pint; x.Pdiff
        |]


module FnGeneration =
    type FnGenerator(symbols:Symbols, probs:TokenIdProbs, lbound:float, ubound:float, maxcount: int) =
    // type FnGenerator(constants:seq<string>, variables:seq<string>, functions:seq<string>, lbound:float, ubound:float, maxcount: int) =
        let cons = Array.ofSeq symbols.constants
        let vars = Array.ofSeq symbols.variables
        let fns  = Array.ofSeq symbols.functions

        let cons_map = Dictionary<string,int>()
        let vars_map = Dictionary<string,int>()
        let fns_map  = Dictionary<string,int>()

        let binaryops = [|Plus; Minus; Star; Cdot; Slash; Accent|]
        let unaryops  = [|Plus; Minus; Log; Ln; Exp; Sqrt|]

        let ps = probs.ToArray()
        let r = Random()
        
        /// updates the values on probabilities vector in [0,1] range
        let update_probs () =
            let s = Array.sum ps
            for i in 0..ps.Length - 1 do ps[i] <- ps[i] / s 
            for i in 1..ps.Length - 1 do ps[i] <- ps[i] + ps[i - 1]  


        let mutable lb = min lbound ubound
        let mutable ub = max lbound ubound
        let mutable max_count = maxcount

        let mutable count = 0
        let mutable n_numbers = 0
        let mutable n_constants = 0
        let mutable n_functions = 0
        let mutable n_variables = 0
        let mutable n_binaries = 0
        let mutable n_unaries = 0
        let mutable n_fracs = 0
        let mutable n_parens = 0
        let mutable n_sums = 0
        let mutable n_prods = 0    
        let mutable n_ints = 0
        let mutable n_diffs = 0

        // Add the symbols to the respective dictionaries, to store the counts
        do 
            cons |> Array.iter (fun s -> cons_map.Add(s, 0))
            vars |> Array.iter (fun s -> vars_map.Add(s, 0))
            fns  |> Array.iter (fun s -> fns_map.Add(s, 0))
            update_probs ()


        let increase_count () = count <- count + 1

        /// select random element from an array
        let rtarget (m:array<'T>) = m[r.Next(m.Length)]            

        /// predicate -Simulated annealing logic for determining if to accept a random number-value
        let predicateN n p = not (if r.NextDouble() < p then true else n > lb && n < ub)

        /// predicate -Simulated annealing logic for determining if to accept a random Symbol
        /// where Symbol is -constant, variable, function
        let predicateV (m:System.Collections.Generic.Dictionary<string,int>) t p =            
            // not (if r.NextDouble() < p then true else m[t] < m.Count * 2)
            false


        let rec createExpr () =
            count <- count + 1
            match r.NextDouble() with          
            | p when count > max_count -> // prevent function getting too large
                match r.Next(4) with
                | 0 -> createNumber lb ub
                | 1 -> createConstant 0.02
                | 2 -> createVariable 0.02
                | 3 -> createFunction 0.04
                | _ -> createNumber lb ub
            | p when p <= ps[0] -> createNumber lb ub
            | p when p <= ps[1] -> createConstant 0.04
            | p when p <= ps[2] -> createVariable 0.035
            | p when p <= ps[3] -> createFunction 0.45
            | p when p <= ps[4] -> createBinary binaryops
            | p when p <= ps[5] -> createUnary unaryops
            | p when p <= ps[6] -> createParen () 
            | p when p <= ps[7] -> createFrac ()
            // | p when p <= ps[8] -> createSum()
            // | p when p <= ps[9] -> createProd()
            // | p when p <= ps[10] -> createInt()
            // | p when p <= ps[11] -> createDiff()
            | _ -> createBinary binaryops       
    
        // implements a Simulated annelaing logic for the randomness of the number
        and createNumber a b = 
            let mutable r_n = (b - a) * r.NextDouble() + a
            n_numbers <- n_numbers + 1
            Number (ref r_n)
                
        /// creates a Constant from the Constants map    
        and createConstant p =
            match cons.Length with
            | 0 -> createExpr ()
            | _ -> 
                let mutable t = rtarget cons
                n_constants <- n_constants + 1
                cons_map[t] <- cons_map[t] + 1
                Constant (ref t)
            
        /// creates a Symbol from the Variables map
        and createVariable p =
            match vars.Length with
            | 0 -> createExpr () 
            | _ -> 
                let mutable t = rtarget vars
                n_variables <- n_variables + 1
                vars_map[t] <- vars_map[t] + 1
                Symbol (ref t)

        /// creates a Symbol from the functions map
        and createFunction p =
            match fns.Length with
            | 0 -> createExpr () 
            | _ ->
                let mutable t = rtarget fns
                n_functions <- n_functions + 1
                fns_map[t] <- fns_map[t] + 1
                Symbol (ref t)

        and createParen () =
            if n_parens < count / 3 then 
                n_parens <- n_parens + 1
                let expr = createExpr ()
                Parenthesized (expr)
            else createExpr ()

        /// creates a random unary Expr node with a TokenId array -op
        and createUnary (ops:array<TokenId>) = 
            let op = rtarget ops
            let expr = createExpr ()
            n_unaries <- n_unaries + 1            
            Unary (expr, ref op)
            
        /// creates a random binary Expr node with a TokenId array -op
        and createBinary (ops:array<TokenId>) =
            let op = rtarget ops
            let lhs = createExpr ()
            let rhs = createExpr ()
            n_binaries <- n_binaries + 1
            Binary (lhs, rhs, ref op)            
        
        and createFrac () = 
            let u = createExpr ()
            let l = createExpr ()
            n_fracs <- n_fracs + 1
            Frac (u, l)


        /// Creates a randomly generated function, with the designated lhs-string tag-name
        member _.fngenerate (fn_name:string) :Expr =
            let binary = createBinary binaryops
            Assignment (fn_name, binary)


    let generatefn (fn_name:string) (symbols:Symbols) (probs:TokenIdProbs) lbound ubound maxnodes :Expr =
        let generator = FnGenerator(symbols, probs, lbound, ubound, maxnodes)
        let mutable fn = generator.fngenerate fn_name
        while (ExprTree.variablesAll fn).Length < 2 do
            fn <- generator.fngenerate fn_name
        fn

    /// parses a BoundExpr from a tex-string
    let fromTeX (symbols:Symbols) (tex:string) = Binder.bind (Parser.parse symbols tex)

    // [<Obsolete>]
    // let generatefnEXT fn_name (symbols:Symbols) (probs:TokenIdProbs) lbound ubound maxnodes :Expr = 
    //     let mutable fn = generatefn fn_name symbols probs lbound ubound maxnodes
    //     while (ExprTree.variablesAll fn).Length < 2 do
    //         fn <- generatefn fn_name symbols probs lbound ubound maxnodes
    //     fn



module Optimization =
    module GeneticAlg =

        type Gene = array<string * Variable>
        type Population = array<Gene>
        
    
        let population (maps:Maps) f (r:Random) (N:int) :Population =                
            let population' = Array.init N (fun _ -> 
                variables f
                |> Array.filter (fun x -> maps.variables.ContainsKey x.Value)
                |> Array.map (fun x -> x.Value)
                |> Array.map (fun x -> x,maps.variables[x])
            )
            for gene in population' do
                for (s,v) in gene do
                    v.V <- ValueSome (v.A + (v.B - v.A) * r.NextDouble())
            population'

        let fitness (maps:Maps) t f (x:array<float>) (y:array<float>) (population':Population) =
            let n = float x.Length
            let fn = Binder.bind f
            let fit = [|
                for gene in population' do
                    for (s,v) in gene do maps.variables[s].V <- v.V                        
                    let mutable err = 0.
                    for yi in y do
                        let yn = Evaluation.eval maps t fn
                        err <- err + (yi - yn) * (yi - yn) / n
                    sqrt err  
            |]
            population'
            |> Array.zip fit
            |> Array.sortBy (fun (fitness_value,_) -> fitness_value)
            |> Array.unzip
           
        let mutate (r:Random) (population':Population) =
            for gene in population' do
                for (s,v) in gene do
                    match r.NextDouble() with
                    | p when p < 0.5 -> v.V <- ValueSome (v.A + (v.B - v.A) * r.NextDouble()) 
                    | p when p > 0.5 -> v.V <- ValueSome (v.B - (v.B - v.A) * r.NextDouble())
                    | _ -> v.V <- ValueSome ((v.B - v.A) * r.NextDouble() + v.A) 
            population'

        let constrain (population':Population) =
            for gene in population' do
                for (s,v) in gene do
                    match v.V with
                    | ValueSome V ->
                        if V < v.A then v.V <- ValueSome v.A
                        if V > v.B then v.V <- ValueSome v.B
                    | ValueNone -> v.V <- ValueSome (v.A + (v.B - v.A) * 0.5)
            population'


        let crossover (r:Random) (population':Population) = 
            let gene_crossover (a:Gene) (b:Gene) (r:Random) = 
                let idx = r.Next(a.Length)
                for i in 0..idx - 1 do a[i] <- b[i]
                for i in idx..a.Length - 1 do b[i] <- a[i]

            let idx = population'.Length / 2
            for i in 0..idx - 1 do
                let a = population'[i + 0]
                let b = population'[i + idx]
                gene_crossover a b r
            population'

        
module Mutation =
    open ExprTree
    open Optimization

    type OptimizationDesc = {
        maps: Maps
        x: array<float>
        y: array<float>        
        err: float
    }


    /// select random element from an array
    let inline private pick (vec:array<'T>) (r:Random) :'T = 
        if vec.Length = 0 then failwith "array is empty" else vec[r.Next(vec.Length)]
    
    /// if possible ensures that f contains specific target else returns None
    let withtarget (t:string) f :option<Expr> =
        let vars = variables f
        if not (vars |> Array.exists (fun x -> x.Value = t)) && vars.Length > 2 then 
            let idx = pick vars[1..] (new Random())
            idx.Value <- t
            // printfn "%s" (latex f)
            Some f
        else None


    let rmse (maps:Maps) (t:string) (f:Binder.BoundExpr) (x:array<float>) (y:array<float>) = 
        let v = maps.variables
        let n = float y.Length
        let mutable err = 0.0
        
        for (xi,yi) in Array.zip x y do
            if xi >= v[t].A && xi <= v[t].B then
                v[t].V <- ValueSome xi
                let yn = Evaluation.eval maps t f
                err <- err + (yi - yn) * (yi - yn) / n
        sqrt err 
        
    // [<Obsolete>]
    // let private rmse (a:array<float>) (b:array<float>) =
    //     let mutable err = 0.
    //     let n = min a.Length b.Length
    //     let t = float n
    //     for i in 0..n - 1 do
    //         let dy = a[i] - b[i]
    //         err <- err + (dy * dy / t)
    //     sqrt err

    // #########################################################
    // ONLY the nodes that containt ref<values> matter to mutate!!!
    // #########################################################

    let mutateNumber (n:ref<float>) (lb:float) (ub:float) (r:Random) =
        n.Value <- (r.NextDouble()) * (ub - lb) + lb


    let mutateConstant (c:ref<string>) (cons:array<string>) (r:Random) = c.Value <- pick cons r

    let mutateVariable (s:ref<string>) (vars:array<string>) (fns:array<string>) (r:Random) =
        match r.NextDouble() with
        | d when d >= 0.2 && vars.Length > 0 -> s.Value <- pick vars r
        | d when d < 0.2 && fns.Length > 0 -> s.Value <- pick fns r
        | _ -> ()

        
    let mutateUnary (u:Expr * ref<TokenId>) (ops:array<TokenId>) (r:Random) = 
        let (_, op) = u
        op.Value <- pick ops r

    let mutateBinary (b:Expr * Expr * ref<TokenId>) (ops:array<TokenId>) (r:Random) = 
        let (_, _, op) = b
        op.Value <- pick ops r


    let optimizefn (pars:OptimizationDesc) N lb ub t f :Expr*float =
        let cons = Array.ofSeq (pars.maps.constants.Keys)
        let vars = Array.ofSeq (pars.maps.variables.Keys)
        let fns  = Array.ofSeq (pars.maps.functions.Keys)
    
        let fn = copy f        // copy of f to mutate, the original f is not mutated
        let mutable fn_out = copy f
        let mutable err0 = Double.MaxValue

        let ns = numbers fn
        let cs = constants fn
        let vs = variablesAll fn
        let us = unaries fn
        let bs = binaries fn
        let r  = Random()

        let uops = [|Plus; Minus; Log; Ln; Exp; Sqrt|]
        let bops = [|Plus; Minus; Star; Cdot; Slash; Accent|]

        if vs.Length > 0 then
            for i in 0..N do
                if i % 2 = 0 && ns.Length > 0 then mutateNumber (pick ns r) lb ub  r
                if i % 3 = 0 && vs.Length > 0 then mutateVariable (pick vs r) vars fns r
                if i % 4 = 0 && bs.Length > 0 then mutateBinary (pick bs r) bops r
                if i % 5 = 0 && cs.Length > 0 then mutateConstant (pick cs r) cons r
                if i % 6 = 0 && us.Length > 0 then mutateUnary (pick us r) uops r
                
                // ensure that targets for evaluation always exists and mutation does not break it
                if not (vs |> Array.exists (fun x -> x.Value = t)) then vs[r.Next(vs.Length)].Value <- t

                // genetic alg to compute more accurately the err
                // let mutable genN   = 0
                // let mutable break' = false
                // while genN < 100 && not break' do
                //     let (fitness, population') =
                //         population
                //         |> GeneticAlg.mutate r
                //         |> GeneticAlg.constrain
                //         |> GeneticAlg.crossover r
                //         |> GeneticAlg.fitness maps t f pars.x pars.y
                //     population <- population'
                    
                    // if fitness[0] < err0 then
                        // err0 <- fitness[0]
                    //     fn_out <- copy fn
                    // printfn "genalg. iter.: %d,  err: %g" genN err0
                        
                //     if fitness[0] < pars.err || fitness[0] > 1e2 then
                //         break' <- true
                        
                //     genN <- genN + 1
                let population = GeneticAlg.population pars.maps fn r 100
                let (fitness,_) = GeneticAlg.fitness pars.maps t fn pars.x pars.y population
                if fitness[0] < err0 then
                    err0 <- fitness[0]
                    fn_out <- copy fn
        fn_out, err0       
                
