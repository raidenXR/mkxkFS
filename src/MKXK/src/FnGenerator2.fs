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

// [<Struct>] 
type BinaryProbs = {
    add:  int
    sub:  int
    mult: int
    div:  int
    pow:  int
}

// [<Struct>]
type UnaryProbs = {
    idnt: int
    neg:  int
    log:  int
    ln:   int
    exp:  int
    abs:  int
    sqrt: int 
    cos:  int
    sin:  int
    tan:  int
}

// [<Struct>]
type TokenIdProbs = {
    numb:  int
    con:   int
    var:   int
    fn:    int
    bin:   int
    unr:   int
    paren: int
    frac:  int
    sum:   int
    prod:  int
    int:   int
    diff:  int  
}

type Probs = {
    unaries: UnaryProbs
    binaries: BinaryProbs
    tokenids: TokenIdProbs
}

module Probabilities =

    /// updates the values on probabilities vector in [0,1] range
    let update (ps:array<float>) =
        let s = Array.sum ps
        for i in 0..ps.Length - 1 do ps[i] <- ps[i] / s 
        for i in 1..ps.Length - 1 do ps[i] <- ps[i] + ps[i - 1]  

    let array (p:obj) :array<float> =
        match p with
        | :? UnaryProbs as u -> 
            Array.map float [|
                u.idnt; u.neg; u.log; u.ln; 
                u.exp; u.abs; u.sqrt; 
                u.cos; u.sin; u.tan
            |]
        | :? BinaryProbs as b ->
            Array.map float [|
                b.add; b.sub; b.mult; b.div; b.pow
            |]
        | :? TokenIdProbs as t ->
            Array.map float [|
                t.numb; t.con; t.var; t.fn
                t.bin; t.unr; t.paren; t.frac
                t.sum; t.prod; t.int; t.diff
            |]            
        | _ -> failwith "inappropriate type"
        

    [<return: Struct>]
    let (|P|_|) id (ps:array<float>) p = 
        if p <= ps[id] then ValueSome true else ValueNone

// type TokenIdProbs = {
//         Pnumber: int
//         Pconstant: int
//         Pvariable: int
//         Pfunction: int
//         Pbinary: int
//         Punary: int
//         Pparen: int
//         Pfrac: int
//         Psum: int
//         Pprod: int
//         Pint: int
//         Pdiff: int
//     } 
//     with 
//         static member Default = {
//             Pnumber = 10
//             Pconstant = 10
//             Pvariable = 10
//             Pfunction = 10
//             Pbinary = 15
//             Punary = 15
//             Pparen = 10
//             Pfrac = 10
//             Psum = 0
//             Pprod = 0
//             Pint = 0
//             Pdiff = 0
//         }
//         member x.ToArray() = Array.map float [|
//             x.Pnumber; x.Pconstant; x.Pvariable; x.Pfunction
//             x.Pbinary; x.Punary; x.Pparen; x.Pfrac
//             x.Psum; x.Pprod; x.Pint; x.Pdiff
//         |]


module FnGeneration =
    type FnGenerator(symbols:Symbols, probs:TokenIdProbs, lbound:float, ubound:float, maxcount: int) =
    // type FnGenerator(constants:seq<string>, variables:seq<string>, functions:seq<string>, lbound:float, ubound:float, maxcount: int) =
        let cons = Array.ofSeq symbols.constants
        let vars = Array.ofSeq symbols.variables
        let fns  = Array.ofSeq symbols.functions

        let cons_map = System.Collections.Generic.Dictionary<string,int>()
        let vars_map = System.Collections.Generic.Dictionary<string,int>()
        let fns_map = System.Collections.Generic.Dictionary<string,int>()

        let binaryops = [|Plus; Minus; Star; Cdot; Slash; Accent|]
        let unaryops = [|Plus; Minus; Log; Ln; Exp; Sqrt|]

        let ps = Probabilities.array probs

        let [<Literal>] numb = 0
        let [<Literal>] con = 0
        let [<Literal>] var = 0
        let [<Literal>] fn = 0
        let [<Literal>] bin = 0
        let [<Literal>] unr = 0
        let [<Literal>] paren = 0
        let [<Literal>] frac = 0
        let [<Literal>] sum = 0
        let [<Literal>] prod = 0
        let [<Literal>] int = 0
        let [<Literal>] diff = 0

        // let ps = {TokenIdProbs.Default with Pnumber = 5}.ToArray()
        // probabilities for each Node-type
        // let ps = 
        //     [|
        //         10 // Pnumber 
        //         10 // Pconstant
        //         10 // Pvariable
        //         10 // Pfunction
        //         15 // Pbinary
        //         15 // Punary
        //         10 // Pparen
        //         10 // Pfrac
        //         0 // Psum
        //         0 // Pprod
        //         0 // Pint
        //         0 // Pdiff
        //         0 // 
        //     |] |> Array.map (float)    
        let r = Random()
        
        let update_probs (probs:array<float>) =
            let s = Array.sum ps
            for i in 0..ps.Length - 1 do ps[i] <- ps[i] / s 
            for i in 1..ps.Length - 1 do ps[i] <- ps[i] + ps[i - 1]  

        [<return: Struct>]
        let (|P|_|) id p = 
            if p <= ps[id] then ValueSome true else ValueNone


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
            Probabilities.update ps


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
            | _ -> createBinary binaryops       
    
        // implements a Simulated annelaing logic for the randomness of the number
        and createNumber a b = 
            let r_number () = (b - a) * r.NextDouble() + a
            let mutable r_n = r_number ()            
            while predicateN r_n 0.075 do r_n <- r_number ()
            n_numbers <- n_numbers + 1
            Number (ref r_n)
                
        /// creates a Constant from the Constants map    
        and createConstant p =
            if cons.Length = 0 then createExpr ()
            else
                let mutable t = rtarget cons
                while predicateV cons_map t p do t <- rtarget cons
                n_constants <- n_constants + 1
                cons_map[t] <- cons_map[t] + 1
                Constant (ref t)
            
        /// creates a Symbol from the Variables map
        and createVariable p =
            if vars.Length = 0 then createExpr () 
            else
                let mutable t = rtarget vars
                while predicateV vars_map t p do t <- rtarget vars
                n_variables <- n_variables + 1
                vars_map[t] <- vars_map[t] + 1
                Symbol (ref t)

        /// creates a Symbol from the functions map
        and createFunction p =
            if fns.Length = 0 then createExpr () 
            else
                let mutable t = rtarget fns
                while predicateV fns_map t p do t <- rtarget fns
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


    // let generatefn 
    //     (fn_name:string) 
    //     (constants:seq<string>) 
    //     (variables:seq<string>) 
    //     (functions:seq<string>) 
    //     lbound 
    //     ubound 
    //     maxnodes :Expr =
    //     let generator = FnGenerator(constants, variables, functions, lbound, ubound, maxnodes)
    //     generator.fngenerate fn_name


    let generatefn (fn_name:string) (symbols:Symbols) (probs:TokenIdProbs) lbound ubound maxnodes :Expr =
        let generator = FnGenerator(symbols, probs, lbound, ubound, maxnodes)
        generator.fngenerate fn_name    

    /// parses a BoundExpr from a tex-string
    let fromtex (symbols:Symbols) (tex:string) = Binder.bind (Parser.parse symbols tex)

    // let generatefnEXT 
    //     (fn_name:string) 
    //     (constans:seq<string>) 
    //     (variables:seq<string>) 
    //     (functions:seq<string>)
    //     (lbound:float)
    //     (ubound:float)
    //     (maxnodes:int) :Expr = 
    let generatefnEXT fn_name (symbols:Symbols) (probs:TokenIdProbs) lbound ubound maxnodes :Expr = 
        let mutable fn = generatefn fn_name symbols probs lbound ubound maxnodes
        while (ExprTree.variablesAll fn).Length < 2 do
            fn <- generatefn fn_name symbols probs lbound ubound maxnodes
        fn

        
module Mutation =
    open ExprTree

    type OptimizationDesc = {
        // constants: Map<string,float>
        // variables: Map<string,Variable>
        // functions: Map<string,Binder.BoundExpr>
        maps: Maps
        x: array<float>
        y: array<float>        
        err: float
    }

            
    // type OptimizationDesc = {
    //     maps: Maps
    //     x: array<float>
    //     y: array<float>        
    //     err: float
    // }
            

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


    let optimizefn (pars:OptimizationDesc) N lb ub t f :Expr*float =
        // let cons = pars.constants.Keys |> Array.ofSeq
        // let vars = pars.variables.Keys |> Array.ofSeq
        // let fns  = pars.functions.Keys |> Array.ofSeq
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
        let r = Random()

        let uops = [|Plus; Minus; Log; Ln; Exp; Sqrt|]
        let bops = [|Plus; Minus; Star; Cdot; Slash; Accent|]
        let yret = Array.zeroCreate<float> pars.x.Length

        if vs.Length > 0 then
            for i in 0..N do
                if i % 2 = 0 && ns.Length > 0 then mutateNumber (pick ns r) lb ub  r
                if i % 3 = 0 && vs.Length > 0 then mutateVariable (pick vs r) vars fns r
                if i % 5 = 0 && cs.Length > 0 then mutateConstant (pick cs r) cons r
                if i % 9 = 0 && us.Length > 0 then mutateUnary (pick us r) uops r
                if i % 4 = 0 && bs.Length > 0 then mutateBinary (pick bs r) bops r
                
                // ensure that targets for evaluation always exists and mutation does not break it
                if not (vs |> Array.exists (fun x -> x.Value = t)) 
                    then vs[r.Next(vs.Length)].Value <- t
                // Evaluation.evalvalues yret pars.constants pars.variables pars.functions t (Binder.bind fn)
                Evaluation.evalvalues yret pars.maps t (Binder.bind fn)
                let err = rmse pars.y yret
                if err < err0 then 
                    fn_out <- copy fn  
                    err0 <- err
        fn_out, err0       
                
