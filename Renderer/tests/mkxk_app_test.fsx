#r "nuget: System.Drawing.Common, 8.0.0"
#r "../bin/Debug/net8.0/mkxkFS.dll"
#r "../bin/Debug/net8.0/Notation.dll"
#r "../bin/Debug/net8.0/Renderer.dll"
#r "../bin/Debug/net8.0/SKCharts.dll"

open System
open System.IO
open MKXK
open ExprTree
open RendererFS


// create some table of variables
let vars = 
    [
        "a0", 10, 120
        "a1", 12, 30
        "a2", 21, 40
        "a3", 11, 200
        "T", 0, 300
        "C_a", 121, 400
        "C_{\\beta}", 1, 20
        "C_A", 1, 40
        "C_B", 12, 40
        "C_{\\gamma}", 110, 200
        "t", 1, 30
        """C_{AB}""", 1, 40
    ] 
    |> List.map (fun (s,a,b) -> s,{A = a; B = b; V = ValueNone})
    |> Map.ofList

// create some table of constants
let cons = Map [
    "N_A", 10.05
    "e", 1e-4
    "k_A", 1e3
    "R", 2.34 * 1e-7
    "E_A", 100
    "A_0", 1.342
    "A_1", 8.097
    "A_2", 1.12345 * 1e-4
    "A_n", 1e7
    "N", 8.235
]

// define some tex strings
let [<Literal>] f0str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
let [<Literal>] f1str = "g(x) = T + k_A - (C_A * 4.3) / C_B - t^2"
let [<Literal>] f6str = "z(x) = A_n + A_2 - (C_A * 4.3) / A_1 - 8^2 + t^2"

let fnbind tex = Binder.bind (Parser.parse tex cons.Keys vars.Keys [])

let fns = Map [
    "f(x)", (fnbind f0str)
    "g(x)", (fnbind f1str)
    "z(x)", (fnbind f6str)
]


let mutable counter = 1

let cout (pair:Expr * float) =
    let struct(i,j) = Console.GetCursorPosition()
    Console.WriteLine $"fn optimized: {counter}"
    Console.SetCursorPosition(0,j)
    counter <- counter + 1
    pair

let [<Literal>] N = 600 // optimization loop
let [<Literal>] L = 600 // no of rng fns

// raw points
let x = Array.init 40 (fun i -> float i)
let y = Array.init 40 (fun i -> 1e-3 * x[i] * x[i] - 0.3 * x[i] + 1.25)
let z = Array.init 40 (fun i -> 1e-2 * x[i] * x[i])

let desc: Mutation.OptimizationDesc = {
    constants = cons
    variables = vars
    functions = fns
    x = x
    y = y  // optimize vs rp0
    err = 1e3
}
let parallel_opt = (Mutation.optimizefn desc N 0.1 1e3 "C_A") >> cout
    
printfn "rng-fn generation #time"
#time
let pipe0 =
    Array.Parallel.init L (fun _ -> FnGeneration.generatefnEXT "f(x)" cons.Keys vars.Keys fns.Keys 0.01 1e5 20)
#time

printfn "\nrng-fn optimization #time"
#time
let pipe1 = 
    pipe0
    |> Array.Parallel.map parallel_opt
    |> Array.sortBy (fun (f,err) -> err)
    |> Array.take 10
#time

let (fns_optimized, errs) = 
    pipe1
    |> Array.unzip


// keep a log of generated fns and their errors
let html = Html.HtmlBuilder()
html
|> Html.header 2 "rng functions"
|> Html.olist (Array.map2 (fun x y -> $"${(latex x)}$      err:{y:N6}") fns_optimized errs)
|> Html.close "functions.html"


let models = [
    Model2.createpoints x y Colors.Navy 4.2f
    Model2.createpoints x z Colors.Purple 4.2f
    Model2.createTeXModel cons vars fns (latex (fns_optimized[0])) "C_A" Colors.Green 2.0f
    Model2.createTeXModel cons vars fns (latex (fns_optimized[1])) "C_A" Colors.Red 2.0f
    Model2.createTeXModel cons vars fns (latex (fns_optimized[2])) "C_A" Colors.Blue 2.0f
    Model2.createTeXModel cons vars fns (latex (fns_optimized[3])) "C_A" Colors.Brown 2.0f
    Model2.createTeXModel cons vars fns (latex (fns_optimized[4])) "C_A" Colors.Silver 2.0f
]

let names = [
    "rp0"
    "rp1"
    "f1(x)"
    "f2(x)"
    "f3(x)"
    "f4(x)"
    "f5(x)"  
]

Model2.setNames names models
 

let renderer = Renderer(cons, vars, fns, models)
renderer.Run()
Console.ReadKey()
