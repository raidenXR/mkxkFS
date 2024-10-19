#r "nuget: System.Drawing.Common, 8.0.0"
#r "../bin/Debug/net8.0/mkxkFS.dll"
#r "../bin/Debug/net8.0/Renderer.dll"
#r "../bin/Debug/net8.0/SKCharts.dll"

open System
open System.IO
open MKXK
open ExprTree
open RendererFS


// create some table of variables
let variables = [
    "a0", 1, 20
    "a1", 1, 30
    "a2", 1, 40
    "a3", 1, 20
    "T", 1, 30
    "C_a", 1, 40
    "C_{\\beta}", 1, 20
    "C_A", 1, 30
    "C_B", 1, 40
    "C_{\\gamma}", 1, 20
    "t", 1, 30
    """C_{AB}""", 1, 40
]
let vars = variables |> List.map (fun (s, a, b) -> (s, {A = a; B = b; V = ValueNone })) |> Map.ofList

// create some table of constants
let constants = [
    "N_A", 10
    "e", 10
    "k_A", 10
    "R", 10
    "E_A", 10
    "A_0", 10
    "A_1", 10
    "A_2", 10
    "A_n", 10
    "N", 10
] 
let cons = constants |> List.map (fun (s, v) -> (s, float v)) |> Map.ofList

// define some tex strings
let [<Literal>] f0str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
let [<Literal>] f1str = "g(x) = T + k_A - (C_A * 4.3) / C_B - t^2"
let [<Literal>] f6str = "z(x) = A_n + A_2 - (C_A * 4.3) / A_1 - 8^2 + t^2"

let fns = 
    [
        f0str[0..3], (Binder.bind (Parser.parse f0str cons.Keys vars.Keys []))
        f1str[0..3], (Binder.bind (Parser.parse f1str cons.Keys vars.Keys []))
        f6str[0..3], (Binder.bind (Parser.parse f6str cons.Keys vars.Keys []))
    ] |> Map.ofList 

let rng_fns = 
    [|
    for i in 0..400 -> 
        (FnGeneration.generatefnEXT "f(x)" cons.Keys vars.Keys fns.Keys 0.01 1e5 20)
        // |> Mutation.withtarget "C_A"
    |] 


let desc: Mutation.OptimizationDesc = {
    constants = cons
    variables = vars
    functions = fns
    x = [|1.0..30.0|]
    y = [|1.0..0.5..15.0|]
    err = 1e3
}
let fns_optimized = rng_fns
                    |> Array.map (Mutation.optimizefn desc 300 0.1 1e5 "C_A")
                    // |> Array.take 10
// |> Array.iter (fun f -> printfn "%s\n" (latex f))
// let fntexmodel = Model2.createTeXModel cons vars fns (latex rng_fns[0]) "\\partial T" x Colors.Orange 2.2f


let html = Html.HtmlBuilder()
html
|> Html.header 2 "rng functions"
|> Html.olist (Array.map (fun x -> $"${(latex x)}$") fns_optimized)
|> Html.close "functions.html"


let x = [|for i in 0..40 -> float i|]

let models = [
    Model2.createpoints x [|for i in 0..40 -> float i|] Colors.Navy 4.2f
    Model2.createpoints x [|for i in 0..40 -> float i / 2.0 + 0.3 * (float i)|] Colors.Purple 4.2f
    Model2.createTeXModel cons vars fns (latex (fns_optimized[0])) "C_A" x Colors.Green 2.0f
    Model2.createTeXModel cons vars fns (latex (fns_optimized[1])) "C_A" x Colors.Red 2.0f
    Model2.createTeXModel cons vars fns (latex (fns_optimized[2])) "C_A" x Colors.Blue 2.0f
    Model2.createTeXModel cons vars fns (latex (fns_optimized[3])) "C_A" x Colors.Brown 2.0f
    Model2.createTeXModel cons vars fns (latex (fns_optimized[4])) "C_A" x Colors.Silver 2.0f
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
