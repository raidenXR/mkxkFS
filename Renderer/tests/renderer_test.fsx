#r "nuget: System.Drawing.Common, 8.0.0"
#r "../bin/Debug/net8.0/mkxkFS.dll"
#r "../bin/Debug/net8.0/Renderer.dll"
#r "../bin/Debug/net8.0/SKCharts.dll"


open System
open MKXK
open ExprTree
open RendererFS


// create some table of variables
let variables = 
    [
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
    |> List.map (fun (s, a, b) -> (s, {A = a; B = b; V = ValueNone })) 
    |> Map.ofList

// create some table of constants
let constants = Map [
    "N_A", 10.
    "e", 10.
    "k_A", 10.
    "R", 10.
    "E_A", 10.
    "A_0", 10.
    "A_1", 10.
    "A_2", 10.
    "A_n", 10.
    "N", 10.
] 

// define some tex strings
let [<Literal>] f0str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
let [<Literal>] f1str = "g(x) = T + k_A - (C_A * 4.3) / C_B - t^2"
// let [<Literal>] f2str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
// let [<Literal>] f3str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
// let [<Literal>] f4str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
// let [<Literal>] f5str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
let [<Literal>] f6str = "z(x) = A_n + A_2 - (C_A * 4.3) / A_1 - 8^2 + t^2"

let s': Symbols = {
    constants = constants.Keys
    variables = variables.Keys
    functions = []
}

let fns = Map [
    f0str, (FnGeneration.fromtex s' f0str)
    f1str, (FnGeneration.fromtex s' f1str)
    f6str, (FnGeneration.fromtex s' f6str)
]

let maps: Maps = {
    constants = constants
    variables = variables
    functions = fns
}

let x = [|for i in 0..40 -> float i|]

let models = [
    Model2.createpoints x [|for i in 0..40 -> float i|] Colors.Navy 4.2f
    // Model2.createline x [|for i in 0..40 -> float i|] Colors.Navy 4.2f
    Model2.createpoints x [|for i in 0..40 -> float i / 2.0 + 0.3 * (float i)|] Colors.Purple 4.2f
    // Model2.createline x [|for i in 0..40 -> float i / 2.0 + 0.3 * (float i)|] Colors.Purple 4.2f
    Model2.createTeXModel maps f0str "C_A" Colors.Brown 2.0f
    Model2.createTeXModel maps f1str "C_A" Colors.Green 2.0f
    Model2.createTeXModel maps f6str "C_A" Colors.Blue 2.0f
]

let bounds = function 
    | Model2.Model2D m -> m.Bounds
    | Model2.TeXModel (tex, f, b, m) -> m.Bounds

let model2 = function
    | Model2.Model2D m -> m
    | Model2.TeXModel (tex,f,b,m) -> m

let _models = models |> List.map (fun x -> model2 x) |> Array.ofList

printfn "model0: %A, %d" _models[0].Bounds _models[0].VerticesCount
printfn "model1: %A, %d" _models[1].Bounds _models[1].VerticesCount
printfn "model2: %A, %d" _models[2].Bounds _models[2].VerticesCount
printfn "model3: %A, %d" _models[3].Bounds _models[3].VerticesCount
printfn "model4: %A, %d" _models[4].Bounds _models[4].VerticesCount

_models[0].Name <- "raw pts 0"
_models[1].Name <- "raw pts 1"
_models[2].Name <- "f(x)"
_models[3].Name <- "g(x)"
_models[4].Name <- "z(x)"


let r = Renderer(maps, models)
r.Run()

Console.ReadKey ()
