#r "nuget: System.Drawing.Common, 8.0.0"
#r "../bin/Debug/net8.0/mkxkFS.dll"
#r "../bin/Debug/net8.0/Renderer.dll"
#r "../bin/Debug/net8.0/SKCharts.dll"


open System
open MKXK
open ExprTree
open RendererFS


// create some table of variables
let variables = [
    "a0", 1, 20
    "a1", 1, 20
    "a2", 1, 20
    "a3", 1, 20
    "T", 1, 20
    "C_a", 1, 20
    "C_{\\beta}", 1, 20
    "C_A", 1, 20
    "C_B", 1, 20
    "C_{\\gamma}", 1, 20
    "t", 1, 20
    """C_{AB}""", 1, 20
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
// let [<Literal>] f2str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
// let [<Literal>] f3str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
// let [<Literal>] f4str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
// let [<Literal>] f5str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
let [<Literal>] f6str = "z(x) = A_n + A_2 - (C_A * 4.3) / A_1 - 8^2 + t^2"

let fns = 
    [
        f0str, (Binder.bind (Parser.parse f0str cons.Keys vars.Keys []))
        f1str, (Binder.bind (Parser.parse f1str cons.Keys vars.Keys []))
        f6str, (Binder.bind (Parser.parse f6str cons.Keys vars.Keys []))
    ] |> Map.ofList 


let r = Renderer(cons, vars, fns, 100)
r.Run()

Console.ReadKey ()
