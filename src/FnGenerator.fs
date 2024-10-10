namespace MKXK
open System
open ExprTree

type FnDescription = {
    fn: Expr
    latex: string
    terms: int
    count: int
    err: float
}

module FnGeneration =
    type FnGenerator(constants:seq<string>, variables:seq<string>, functions:seq<string>, lbound:float, ubound:float, maxcount: int) =
        let cons = constants |> Array.ofSeq
        let vars = variables |> Array.ofSeq
        let fns = functions |> Array.ofSeq

        let cons_map = System.Collections.Generic.Dictionary<string,int>()
        let vars_map = System.Collections.Generic.Dictionary<string,int>()
        let fns_map = System.Collections.Generic.Dictionary<string,int>()

        let binaryops = [|Plus; Minus; Star; Cdot; Slash; Accent|]
        let unaryops = [|Plus; Minus; Log; Ln; Exp; Sqrt|]

        // probabilities for each Node-type
        let ps = 
            [|
                10 // Pnumber 
                10 // Pconstant
                10 // Pvariable
                10 // Pfunction
                15 // Pbinary
                15 // Punary
                10 // Pparen
                10 // Pfrac
                0 // Psum
                0 // Pprod
                0 // Pint
                0 // Pdiff
                0 // 
            |] |> Array.map (float)    
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
            fns |> Array.iter (fun s -> fns_map.Add(s, 0))
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
            match r.NextDouble() with          
            | p when count > max_count -> // prevent function getting too large
                match r.Next(4) with
                | 0 -> createNumber lb ub
                | 1 -> createConstant 0.02
                | 2 -> createVariable 0.02
                | 3 -> createFunction 0.04
                | _ -> createNumber lb ub
            | p when p <= ps[0] -> createNumber lb ub
            | p when p > ps[0] && p <= ps[1] -> createConstant 0.04
            | p when p > ps[1] && p <= ps[2] -> createVariable 0.035
            | p when p > ps[2] && p <= ps[3] -> createFunction 0.45
            | p when p > ps[3] && p <= ps[4] -> createBinary binaryops
            | p when p > ps[4] && p <= ps[5] -> createUnary unaryops
            | p when p > ps[5] && p <= ps[6] -> createParen () 
            | p when p > ps[6] && p <= ps[7] -> createFrac ()
            | _ -> createBinary binaryops       
    
        // implements a Simulated annelaing logic for the randomness of the number
        and createNumber a b = 
            // printfn "to create Number node"
            let r_number () = (b - a) * r.NextDouble() + a
            let mutable r_n = r_number ()            
            while predicateN r_n 0.075 do r_n <- r_number ()
            count <- count + 1
            n_numbers <- n_numbers + 1
            // printfn "Number node -created"
            Number (ref r_n)
                
        /// creates a Constant from the Constants map    
        and createConstant p =
            // printfn "to create Constant node"
            if cons.Length = 0 then createExpr ()
            else
                let mutable t = rtarget cons
                while predicateV cons_map t p do t <- rtarget cons
                count <- count + 1
                n_constants <- n_constants + 1
                cons_map[t] <- cons_map[t] + 1
                // printfn "Constant node -created"
                Constant (ref t)
            
        /// creates a Symbol from the Variables map
        and createVariable p =
            // printfn "to create Variable node"
            if vars.Length = 0 then createExpr () 
            else
                let mutable t = rtarget vars
                while predicateV vars_map t p do t <- rtarget vars
                count <- count + 1
                n_variables <- n_variables + 1
                vars_map[t] <- vars_map[t] + 1
                // printfn "Variable node -created"
                Symbol (ref t)

        /// creates a Symbol from the functions map
        and createFunction p =
            // printfn "to create Function node"
            if fns.Length = 0 then createExpr () 
            else
                let mutable t = rtarget fns
                while predicateV fns_map t p do t <- rtarget fns
                count <- count + 1
                n_functions <- n_functions + 1
                fns_map[t] <- fns_map[t] + 1
                // printfn "Function node -created"
                Symbol (ref t)

        and createParen () =
            // printfn "to create Parenthesized node"
            if n_parens < count / 3 then 
                count <- count + 1
                n_parens <- n_parens + 1
                // printfn "Parenthesized node -created"
                let expr = createExpr ()
                Parenthesized (expr)
            else createExpr ()

        /// creates a random unary Expr node with a TokenId array -op
        and createUnary (ops:array<TokenId>) = 
            // printfn "to create Unary node"
            let op = rtarget ops
            let expr = createExpr ()
            count <- count + 1
            n_unaries <- n_unaries + 1            
            // printfn "Unary node -created"
            Unary (expr, ref op)
            
        /// creates a random binary Expr node with a TokenId array -op
        and createBinary (ops:array<TokenId>) =
            // printfn "to create Binary node"
            let op = rtarget ops
            let lhs = createExpr ()
            let rhs = createExpr ()
            count <- count + 1
            n_binaries <- n_binaries + 1
            // printfn "Binary node -created"
            Binary (lhs, rhs, ref op)            
        
        and createFrac () = 
            // printfn "to create Frac node"
            let u = createExpr ()
            let l = createExpr ()
            count <- count + 1
            n_fracs <- n_fracs + 1
            // printfn "Frac node -created"
            Frac (u, l)


        // ctors
        new(cons:seq<string>, vars:seq<string>, lbound:float, ubound:float) = FnGenerator(cons, vars, [], lbound, ubound, 40) 
        
        // member val Count = 0 with get, set
        // member val N_unaries = 0 with get, set
        // member val N_binaries = 0 with get, set
        // member val N_numbers = 0 with get, set
        // member val N_variables = 0 with get, set
        // member val N_constants = 0 with get, set
        // member val N_functions = 0 with get, set
        // member val N_parens = 0 with get, set
        // member val Numbers_range: (float * float) = (0., 1.) with get, set        

        /// Creates a randomly generated function, with the designated lhs-string tag-name
        member _.fngenerate (fn_name:string) :Expr =
            let binary = createBinary binaryops
            Assignment (fn_name, binary)



    let generatefn (fn_name:string) (constants:seq<string>) (variables:seq<string>) (functions:seq<string>) lbound ubound maxnodes :Expr =
        let generator = FnGenerator(constants, variables, functions, lbound, ubound, maxnodes)
        generator.fngenerate fn_name


        
module Mutation =
    open ExprTree

    type OptimizationDesc = {
        constants: Map<string,float>
        variables: Map<string,Variable>
        functions: Map<string,Binder.BoundExpr>
        x: array<float>
        y: array<float>        
        err: float
    }

    
    /// select random element from an array
    let inline private pick (vec:array<'T>) (r:Random) :'T = 
        if vec.Length = 0 then failwith "array is empty" else vec[r.Next(vec.Length)]


    let private rmse (a:array<float>) (b:array<float>) =
        let mutable err = 0.
        let n = min a.Length b.Length
        let t = float n
        for i in 0..n - 1 do
            let dy = a[i] - b[i]
            err <- err + (dy * dy / t)
        sqrt err

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


    let optimizefn (pars:OptimizationDesc) N lb ub t f :Expr =
        let cons = pars.constants.Keys |> Array.ofSeq
        let vars = pars.variables.Keys |> Array.ofSeq
        let fns = pars.functions.Keys |> Array.ofSeq
    
        let ns = numbers f
        let cs = constants f
        let vs = variables f
        let us = unaries f
        let bs = binaries f
        let r = Random()

        let uops = [|Plus; Minus; Log; Ln; Exp; Sqrt|]
        let bops = [|Plus; Minus; Star; Cdot; Slash; Accent|]
        let yret = Array.zeroCreate<float> pars.x.Length

        let fn = copy f        // copy of f to mutate, the original f is not mutated
        let mutable fn_out = copy f
        let mutable err0 = Double.MaxValue

        if vs.Length > 2 then
            for i in 0..N do
                if i % 2 = 0 && ns.Length > 0 then mutateNumber (pick ns r) lb ub  r
                if i % 3 = 0 && vs.Length > 1 then mutateVariable (pick vs r) vars fns r
                if i % 5 = 0 && cs.Length > 0 then mutateConstant (pick cs r) cons r
                if i % 9 = 0 && us.Length > 0 then mutateUnary (pick us r) uops r
                if i % 4 = 0 && bs.Length > 0 then mutateBinary (pick bs r) bops r
                
                Evaluation.evalvalues pars.x yret pars.constants pars.variables pars.functions t (Binder.bind fn)
                let err = rmse pars.y yret
                if err < err0 then 
                    fn_out <- copy fn  
                    err0 <- err
        fn_out       
                
