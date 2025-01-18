#load "../src/expressions.fs"
#load "../src/Lexer.fs"
#load "../src/Parser.fs"
#load "../src/FnGenerator.fs"
#load "../src/Gnuplot.fs"

open System
open MKXK
open ExprTree
open Plotting

let variables = [
    "t", 0, 100
]

let fstr0 = @"T = 2*t + 3.2"
let fstr1 = @"f(x) = 1.3 * \ode{T}{t}"
let fstr2 = @"f(x) = 1.3 * \int{T}dt"
let fstr3 = @"f(x) = 1.3 * \int_{4}^{8}{T}dt"

let s': Symbols = {
    constants = []
    variables = ["t"]
    functions = ["T"]
}

let f0 = Binder.bind (Parser.parse s' fstr0)
let f1 = Binder.bind (Parser.parse s' fstr1)
let f2 = Binder.bind (Parser.parse s' fstr2)
let f3 = Binder.bind (Parser.parse s' fstr3)


// printfn "%s" (latex (Parser.parse s' fstr2))

let maps: Maps = {
    constants = Map []
    variables = variables |> List.map (fun (s,a,b) -> s,{A = a; B = b; V = ValueNone}) |> Map
    functions = Map ["T", f0]
}

let xvalues = Array.zeroCreate<float> 100
let yvalues = Array.zeroCreate<float> 100
Evaluation.evalvaluesXY xvalues yvalues maps "t" f1

let gnu = Gnuplot()
gnu
|> Gnuplot.datablockXY xvalues yvalues "T"
|>> "plot $T"
|> Gnuplot.run

// printfn "%A" yvalues 
// printfn "%A" xvalues 


let xvalues2 = Array.zeroCreate<float> 100
let yvalues2 = Array.zeroCreate<float> 100
Evaluation.evalvaluesXY xvalues yvalues maps "t" f3

let gnu2 = Gnuplot()
gnu2
|> Gnuplot.datablockXY xvalues yvalues "T"
|>> "plot $T"
|> Gnuplot.run

// printfn "%A" xvalues 
// printfn "%A" yvalues 
Console.ReadKey()
