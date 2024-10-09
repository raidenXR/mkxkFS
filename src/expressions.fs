namespace MKXK
open System
open System.Runtime

[<Struct>]
type TokenId =
    | Plus
    | Minus
    | Star 
    | Slash
    | Equal
    | Pipe
    | Number
    | Accent
    | Underscore
    | Whitespace
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Identifier
    | Constant
    | Literal
    | Comment
    | Frac
    | Cdot
    | Log
    | Exp
    | Ln
    | Sqrt
    | Cos
    | Sin
    | Tan
    | Sum
    | Prod
    | Int
    | Diff
    | ODE
    | PDE
    | Text
    | Diacritical
    | Scaler
    | Enclosure
    | Symbol
    | MathOperator
    | Binary
    | Keyword
    | Bad
    | Eof

    override x.ToString() =
        match x with
        | Plus -> "+"
        | Minus -> "-"
        | Star -> "*"
        | Cdot -> "\\cdot"
        | Slash -> "/"
        | Frac -> "÷"
        | Accent -> "^"
        | Equal -> "="
        | LeftParen -> "("
        | RightParen -> ")"
        | LeftBrace -> "{"
        | RightBrace -> "}"
        | Pipe -> "|"
        | Log -> "\\log"
        | Ln -> "\\ln"
        | Exp -> "\\exp"
        | Sqrt -> "\\sqrt"
        | Cos -> "\\cos"
        | Sin -> "\\sin"
        | Tan -> "\\tan"
        | ODE -> "\\ode"
        | PDE -> "\\pde"
        | _ -> "not implemented"
        

[<Struct>]
type Token(pos: int, str: string, id: TokenId) =
    member _.Pos = pos
    member _.Str = str
    member _.Id = id


// [<Struct>]
// type Variable(a: float, b: float, v: voption<float>) =
//     member _.A = a
//     member _.B = b
//     member _.V = v
//     new(a: float, b: float) = Variable(a, b, ValueNone)

type Variable = {
    A: float
    B: float
    mutable V: voption<float>
}


module ExprTree =                
    type Expr = 
        | Number of ref<float>
        | Constant of ref<string>
        | Symbol of ref<string>
        | Assignment of string * Expr
        | Parenthesized of Expr
        | Braces of Expr
        | Frac of Expr * Expr
        | Enclosure of Expr
        | Binary of Expr * Expr * ref<TokenId>
        | Unary of Expr * ref<TokenId>
        | Sum of Expr * Expr * Expr
        | Prod of Expr * Expr * Expr
        | Int of Expr * Expr * Expr
        | Diff of Expr * Expr
        | ODE of Expr * Expr
        | PDE of Expr * Expr
        | ExprNone

        override x.ToString() =
            match x with 
            | Number n -> "Number:"
            | Constant c -> "Constant:"
            | Symbol s -> "Symbol:"
            | Assignment (l, r) -> "Assignment"
            | Binary (l, r, op) -> "Binary"
            | Unary (o, op) -> "Unary"
            | Parenthesized _ -> "Parenthesised"
            | Braces b -> "Braces"
            | Frac (u, l) -> "Frac"
            | Enclosure e -> "Enclosure"
            | Sum (i, n, f) -> "\\sum"
            | Prod (i, n, f) -> "\\prod"
            | Int (i, n, f) -> "\\int"
            | Diff (a, b) -> "\\diff"
            | ODE (a, b) -> "\\ode"
            | PDE (a, b) -> "\\pde"
            | _ -> $"{x} not implemented"


    let binaryOpPrecedence = function 
        | Accent -> 6
        | Star| Slash| Cdot -> 5
        | Plus| Minus -> 4
        | Equal -> 3
        | _ -> 0

    let unaryOpPrecedence = function
        | Log| Ln| Exp| Sqrt -> 7
        | Plus| Minus -> 6
        | _ -> 0


    let children = function
        | Number n -> []
        | Constant c -> []
        | Symbol s -> []
        | Assignment (l, r) -> [Symbol (ref l); r]
        | Parenthesized p -> [p]
        | Braces b -> [b]
        | Frac (u, l) -> [u; l]
        | Enclosure e -> [e]
        | Binary (l, r, op) -> [l; r]
        | Unary (o, op) -> [o]
        | Sum (i, n, f) -> [i; n; f]
        | Prod (i, n, f) -> [i; n; f]
        | Int (a, b, f) -> [a; b; f]
        | Diff (a, b) -> [a; b]
        | ODE (a, b) -> [a; b]
        | PDE (a, b) -> [a; b]
        | ExprNone -> [] 


    let count f :int =
        // let mutable acc = 0
        // let rec count_nodes f =
        //     acc <- acc + 1
        //     match children f with 
        //     | h::_ -> count_nodes h
        //     | [] -> ()
        // count_nodes f
        // acc
        let mutable acc = 0
        let rec count_nodes n =
            acc <- acc + 1
            for child in (children n) do 
                count_nodes child
        count_nodes f
        acc


    let nodes f :seq<Expr> =
        // let rec traverse nodes f =
        //     match children f with
        //     | h::t -> traverse (h::nodes) h
        //     | [] -> List.rev nodes         
        // traverse [] f
        let len = count f
        let buffer = Array.create len ExprNone
        let mutable i = 0
        let rec tranversal n = 
            for n in children n do 
                buffer[i] <- n
                i <- i + 1
                tranversal n
        tranversal f
        Seq.ofArray buffer[0..(i - 1)]


    let latex f :string =
        let sb = System.Text.StringBuilder(1024)
        let write (s: string) = ignore (sb.Append s)

        let rec to_latex = function
            | Number n -> write (n.Value.ToString("N3"))
            | Constant c -> write c.Value
            | Symbol s -> write s.Value
            | Assignment (l, r) -> write $"{l} = "; to_latex r
            | Binary (l, r, op) -> 
                to_latex l; write $" {string op.Value} "
                match op.Value with
                | Accent -> write "{"; to_latex r; write "}"
                | _ -> to_latex r                
            | Unary (o, op) ->
                write (string op.Value)
                match op.Value with
                | Log | Ln | Sqrt -> write "{"; to_latex o; write "}"
                | Exp -> write "("; to_latex o; write ")"
                | _ -> to_latex o 
            | Parenthesized p -> write "("; to_latex p; write ")"
            | Braces b -> write "{"; to_latex b; write "}"
            | Frac (u, l) -> 
                write "\\frac{"
                to_latex u
                write "}{"
                to_latex l
                write "}"
            | Enclosure e -> write "|"; to_latex e; write "|"
            | Sum (i, n, f) -> 
                write "\\sum_{"
                to_latex i
                write "}^{"
                to_latex n
                write "}"
                to_latex f            
            | Prod (i, n, f) -> 
                write "\\prod_{"
                to_latex i
                write "}^{"
                to_latex n
                write "}"
                to_latex f
            | Int (a, b, f) ->
                write "\\int_{"
                to_latex a
                write "}^{"
                to_latex b
                write "}"
                to_latex f
            | Diff (u, l) ->
                failwith "Diff is a special case with a macro. TO BE implemented later" // OBSOLETE ?
            | ODE (a, b) -> 
                write "\\ode{"
                to_latex a
                write "}{"
                to_latex b
                write "}"
            | PDE (a, b) -> 
                write "\\pde{"
                to_latex a
                write "}{"
                to_latex b
                write "}"
            | _ -> failwith "node not implemented yet"

        to_latex f
        string sb

                            
    let tree f: string =
        let sb = System.Text.StringBuilder(1024)
        let write (s: string) = ignore (sb.Append s)

        let rec print_tree indent is_last f =
            let marker = if is_last then "└──" else "├──"
            write indent
            write marker
            write (string f)
            write " "

            match f with
            | Number n -> write (n.Value.ToString("N4"))
            | Constant c -> write c.Value
            | Symbol s -> write s.Value 
            | Assignment _ -> write "="
            | Binary (_, _, op) -> write (string op.Value)
            | Unary (_, op) -> write (string op.Value)
            | Sum _ -> write "\\sum"
            | Prod _ -> write "\\prod"
            | Int _ -> write "\\int"
            | Diff _ -> write "\\diff"
            | ODE _ -> write "\\ode"
            | PDE _ -> write "\\pde"
            | _ -> write ""
            // | _ -> failwith $"{f} not implemented yet"

            // write "\n"
            // let indent' = indent + if is_last then "    " else "|   "
            // match children f with
            // | h::t when t.Length > 0 -> print_tree indent' true h            
            // | _ -> ()
            write "\n"
            let indent' = indent + if is_last then "    " else "|   "
            let children' = children f
            let last_child = if children'.Length = 0 then None else Some (List.last children')
            if last_child.IsSome then                            
                for child in children' do
                    print_tree indent' (child = last_child.Value) child

        print_tree "" true f
        string sb

        
    let numbers (f:Expr) = 
        nodes f 
        |> Array.ofSeq 
        |> Array.filter (function | Number _ -> true | _ -> false)
        |> Array.map (function | Number n -> n | _ -> failwith "expr is not Number")

    let constants (f:Expr) = 
        nodes f 
        |> Array.ofSeq 
        |> Array.filter (function | Constant _ -> true | _ -> false)
        |> Array.map (function | Constant c -> c | _ -> failwith "expr is not Constant")
                
    let variables (f:Expr) = 
        nodes f 
        |> Array.ofSeq 
        |> Array.filter (function | Symbol _ -> true | _ -> false) 
        |> Array.map (function | Symbol v -> v | _ -> failwith "expr is not Variable")
        |> Array.distinct
        |> Array.tail    // skip the first symbol which is the identifier of the function
        
    let binaries (f:Expr) = 
        nodes f 
        |> Array.ofSeq 
        |> Array.filter (function | Binary _ -> true | _ -> false)
        |> Array.map (function | Binary (l,r,op) -> (l,r,op) | _ -> failwith "expr is not Binary")
        
    let unaries (f:Expr) = 
        nodes f 
        |> Array.ofSeq 
        |> Array.filter (function | Unary _ -> true | _ -> false)
        |> Array.map (function | Unary (o,op) -> (o, op) | _ -> failwith "expr is not Unary")

        
    // #########################################################
    // ONLY the nodes that containt ref<values> matter to mutate!!!
    // #########################################################

    // let fractions (f:Expr) = 
    //     nodes f 
    //     |> Array.ofSeq 
    //     |> Array.filter (function | Frac _ -> true | _ -> false)
    //     |> Array.map (function | Frac (u,l) -> (u,l) | _ -> failwith "expr is not Frac")
        
    // let parens (f:Expr) = 
    //     nodes f 
    //     |> Array.ofSeq 
    //     |> Array.filter (function | Parenthesized _ -> true | _ -> false)
    //     |> Array.map (function | Parenthesized e -> e | _ -> failwith "expr is not Parenthesized")
        
    // let sums (f:Expr) = 
    //     nodes f 
    //     |> Array.ofSeq 
    //     |> Array.filter (function | Sum _ -> true | _ -> false)
    //     |> Array.map (function | Sum (i,n,f) -> (i,n,f) | _ -> failwith "expr s not Sum")
        
    // let prods (f:Expr) = 
    //     nodes f 
    //     |> Array.ofSeq 
    //     |> Array.filter (function | Prod _ -> true | _ -> false)
    //     |> Array.map (function | Prod (i,n,f) -> (i,n,f) | _ -> failwith "expr is not Prod")
        
    // let ints (f:Expr) = 
    //     nodes f 
    //     |> Array.ofSeq 
    //     |> Array.filter (function | Int _ -> true | _ -> false)
    //     |> Array.map (function | Int (a,b,f) -> (a,b,f) | _ -> failwith "expr is not Int")
        
    // let diffs (f:Expr) = 
    //     nodes f 
    //     |> Array.ofSeq 
    //     |> Array.filter (function | Diff _ -> true | _ -> false)
    //     |> Array.map (function | Diff (a,b) -> (a,b) | _ -> failwith "expr is not Diff")
        
    // let odes (f:Expr) = 
    //     nodes f 
    //     |> Array.ofSeq 
    //     |> Array.filter (function | ODE _ -> true | _ -> false)
    //     |> Array.map (function | ODE (a,b) -> (a,b) | _ -> failwith "expr is not ODE")
        
    // let pdes (f:Expr) = 
    //     nodes f 
    //     |> Array.ofSeq 
    //     |> Array.filter (function | PDE _ -> true | _ -> false)
    //     |> Array.map (function | PDE (a,b) -> (a,b) | _ -> failwith "expr is not PDE")

    let countNumbers (fn_nodes:seq<Expr>) =
        fn_nodes
        |> Seq.filter (function Number _ -> true | _ -> false)
        |> Seq.length

    let countVariables (fn_nodes:seq<Expr>) =
        fn_nodes
        |> Seq.filter (function Symbol _ -> true | _ -> false)
        |> Seq.length

    let countBinaries (fn_nodes:seq<Expr>) =
        fn_nodes
        |> Seq.filter (function Binary _ -> true | _ -> false)
        |> Seq.length

    let countUnaries (fn_nodes:seq<Expr>) =
        fn_nodes
        |> Seq.filter (function Unary _ -> true | _ -> false)
        |> Seq.length

    let countFractions (fn_nodes:seq<Expr>) =
        fn_nodes
        |> Seq.filter (function Frac _ -> true | _ -> false)
        |> Seq.length



module Binder = 
    open ExprTree
    
    type BinaryOp =
        | Addition = 0
        | Subtraction = 1
        | Multiplication = 2
        | Division = 3
        | Power = 4

    type UnaryOp =
        | Identity = 0
        | Negation = 1
        | Log = 2
        | Ln = 3
        | Exp = 4
        | Abs = 5
        | Sqrt = 6
        | Cos = 7
        | Sin = 8
        | Tan = 9

    type BoundExpr =
        | Number of float
        | Constant of string
        | Symbol of string
        | Assignment of string * BoundExpr
        | Binary of BoundExpr * BoundExpr * BinaryOp
        | Unary of BoundExpr * UnaryOp
        | Sum of int * int * BoundExpr
        | Prod of int * int * BoundExpr
        | Int of float * float * BoundExpr
        | Diff of BoundExpr * BoundExpr
        | ODE of BoundExpr * BoundExpr
        | PDE of BoundExpr * BoundExpr
        | BoundExprNone

    let children = function
        | Assignment (l, r) -> [r]
        | Binary (l, r, op) -> [l; r]
        | Unary (o, op) -> [o]
        | Sum (i, n, f) -> [f]
        | Prod (i, n, f) -> [f] 
        | Int (a, b, f) -> [f]
        | Diff (a, b) -> [a; b]
        | ODE (a, b) -> [a; b]
        | PDE (a, b) -> [a; b]
        | _ -> []

    let rec bind = function
        | Expr.Number n -> Number n.Value
        | Expr.Constant c -> Constant c.Value
        | Expr.Symbol s -> Symbol s.Value
        | Expr.Assignment (l, r) -> Assignment (l, bind r) 
        | Expr.Parenthesized p -> bind p
        | Frac (u, l) -> 
            let upper = bind u
            let lower = bind l
            Binary (upper, lower, BinaryOp.Division)
        | Expr.Enclosure e -> 
            let operand = bind e
            Unary (operand, UnaryOp.Abs)
        | Expr.Braces b -> bind b
        | Expr.Binary (l, r, op) -> 
            let lhs = bind l
            let rhs = bind r
            let op' = match op.Value with
                        | Plus -> BinaryOp.Addition
                        | Minus -> BinaryOp.Subtraction
                        | Star -> BinaryOp.Multiplication
                        | Cdot -> BinaryOp.Multiplication
                        | Slash -> BinaryOp.Division
                        | TokenId.Frac -> BinaryOp.Division
                        | Accent -> BinaryOp.Power
                        | _ -> failwith $"{op} not implemented yet"
            Binary (lhs, rhs, op')
        | Expr.Unary (o, op) ->
            let operand = bind o
            let op' = match op.Value with
                        | Plus -> UnaryOp.Identity
                        | Minus -> UnaryOp.Negation
                        | Log -> UnaryOp.Log
                        | Ln -> UnaryOp.Ln
                        | Exp -> UnaryOp.Exp
                        | Sqrt -> UnaryOp.Sqrt
                        | _ -> failwith $"{op} not implemented yet"
            Unary (operand, op')          
        | Expr.Sum (i, n, f) -> 
            let i' = match i with | Expr.Number n -> int n.Value | _ -> failwith "wrong epxr" 
            let n' = match n with | Expr.Number n -> int n.Value | _ -> failwith "wrong epxr" 
            let f' = bind f
            Sum (i', n', f') 
        | Expr.Prod (i, n, f) ->
            let i' = match i with | Expr.Number n -> int n.Value | _ -> failwith "wrong epxr" 
            let n' = match n with | Expr.Number n -> int n.Value | _ -> failwith "wrong epxr" 
            let f' = bind f
            Prod (i', n', f')
        | Expr.Int (a, b, f) -> 
            let a' = match a with | Expr.Number n -> int n.Value | _ -> failwith "wrong epxr" 
            let b' = match b with | Expr.Number n -> int n.Value | _ -> failwith "wrong epxr" 
            let f' = bind f
            Int (a', b', f')
        | Expr.Diff (u, l) ->
            let upper = bind u
            let lower = bind l
            Diff (upper, lower)            
        | Expr.ODE (a, b) ->
            let upper = bind a
            let lower = bind b
            ODE (upper, lower)            
        | Expr.PDE (a, b) ->
            let upper = bind a
            let lower = bind b
            PDE (upper, lower)            
        | _ -> failwith "not implemented case"
    



module Evaluation =
    open Binder
    
    let rec eval (cons: Map<string,float>) (vars: Map<string,Variable>) (fns: Map<string,BoundExpr>) t f :float =
        match f with 
        | Number n -> n
        | Constant c -> cons[c]
        | Symbol s -> 
            if vars.ContainsKey s then
                let v = vars[s]
                match v.V with 
                | ValueSome s -> s
                | ValueNone -> v.A + (v.B - v.A) / 2.
            elif fns.ContainsKey s then eval cons vars fns t (fns[t])
            else failwith $"symbol -{s}- not found in maps"
        | Assignment (l, r) -> eval cons vars fns t r
        | Binary (l, r, op) -> 
            let lhs = eval cons vars fns t l
            let rhs = eval cons vars fns t r
            match op with 
            | BinaryOp.Addition -> lhs + rhs
            | BinaryOp.Subtraction -> lhs - rhs
            | BinaryOp.Multiplication -> lhs * rhs
            | BinaryOp.Division -> lhs / rhs
            | BinaryOp.Power -> lhs ** rhs
            | _ -> failwith "not implemented"
        | Unary (o, op) -> 
            let operand = eval cons vars fns t o
            match op with
            | UnaryOp.Identity -> operand
            | UnaryOp.Negation -> -operand
            | UnaryOp.Log -> log10 operand
            | UnaryOp.Ln -> log operand
            | UnaryOp.Exp -> exp operand
            | UnaryOp.Abs -> abs operand
            | UnaryOp.Sqrt -> sqrt operand
            | _ -> failwith "not implemented yet"
        | Sum (i, n, f) -> 
            let mutable i' = i
            let mutable res = 0.
            while i' < n do 
                res <- res + (eval cons vars fns t f)
                i' <- i' + 1
            res
        | Prod (i, n, f) -> 
            let mutable i' = i
            let mutable res = 0.
            while i' < n do
                res <- res + (eval cons vars fns t f)
                i' <- i' + 1
            res
        | Int (a, b, f) ->
            let dx = (b - a) / 1000.
            let mutable x = a
            let mutable res = 0.
            vars["dx"].V <- ValueSome 1.0
            vars["x"].V <- ValueSome x

            for i in 0..1000 do
                x <- x + dx
                vars["x"].V <- ValueSome x
                let fx = eval cons vars fns t f
                res <- dx * if i % 2 = 0 then fx * 4. / 3. else fx * 2. / 3.            
            // lhs
            res <- res + ((eval cons vars fns t f) * dx / 3.)
            // rhs
            res <- res + ((eval cons vars fns t f) * dx / 3.)
            res
        | Diff (u, l) -> 
            let h = 0.001
            let v = vars[t]
            let x = match v.V with | ValueSome s -> s | ValueNone -> v.A + (v.B - v.A) / 2.
            let a = x - h / 2.
            let b = x + h / 2.
            
            vars[t].V <- ValueSome a
            let fa = eval cons vars fns t u
            vars[t].V <- ValueSome b
            let fb = eval cons vars fns t l
            (fb - fa) / h
        | ODE (a, b) -> 
            // solve ODE with RK4
            // this solves over a series of x-values.
            // For single value run Diff  !!!!!
            let mutable h = 0.
            let mutable t' = 0.
            let mutable a = 0.
            let mutable b = 10.
            let y = Array.zeroCreate<float> 2
            let ydumb = Array.zeroCreate<float> 2
            let freturn = Array.zeroCreate<float> 2
            let k1 = Array.zeroCreate<float> 2
            let k2 = Array.zeroCreate<float> 2
            let k3 = Array.zeroCreate<float> 2
            let k4 = Array.zeroCreate<float> 2
            let mutable i = 0
            let mutable n = 100

            h <- (b - a) / (float n)
            t' <- a

            // initial y-evaluation
            y[0] <- eval cons vars fns t f
            vars[t].V <- ValueSome (vars[t].A + h)
            y[1] <- eval cons vars fns t f

            // capture the evaluation step inside the f-function block
            let fn (tx: float, y: array<float>, fret:array<float>) = 
                vars[t].V <- ValueSome tx
                fret[0] <- y[1]
                fret[1] <- eval cons vars fns t f
        
            while t' < b do
                if ((t' + h) > b) then h <- b - t'
                fn (t', y, freturn)
                k1[0] <- h * freturn[0]
                k1[1] <- h * freturn[1]
                for i in 0..1 do ydumb[i] <- y[i] + k1[i] / 2.
                fn (t' + h / 2., ydumb, freturn)
                k2[0] <- h * freturn[0]
                k2[1] <- h * freturn[1]
                for i in 0..1 do ydumb[i] <- y[i] + k2[i] / 2.
                fn (t' + h / 2., ydumb, freturn)
                k3[0] <- h * freturn[0]
                k3[1] <- h * freturn[1]
                for i in 0..1 do ydumb[i] <- y[i] + k3[i]
                fn (t' + h, ydumb, freturn)
                k4[0] <- h * freturn[0]
                k4[1] <- h * freturn[1]
                for i in 0..1 do y[i] <- y[i] + (k1[i] + 2. * (k2[i] + k3[i]) + k4[i]) / 6.
                t' <- t' + h
            y[0]
        | PDE (a, b) -> failwith "not implemented yet"
        | _ -> failwith "not implemented yet"
            

    /// apply the eval function on a series of x-values and store the results on a y-array (target)
    let evalvalues (x:array<float>) (y:array<float>) cons (vars:Map<string,Variable>) fns t f :unit =
        for i in 0..y.Length - 1 do
            vars[t].V <- ValueSome x[i]
            y[i] <- eval cons vars fns t f
            
