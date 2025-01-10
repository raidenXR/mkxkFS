#r "../bin/Debug/net8.0/mkxkFS.dll"
#r "../bin/Debug/net8.0/Renderer.dll"
#r "../bin/Debug/net8.0/SKCharts.dll"

#r "nuget: Avalonia, 11.2.3"
#r "nuget: Avalonia.Desktop, 11.2.3"
#r "nuget: Avalonia.Themes.Fluent, 11.2.3"
#r "nuget: Avalonia.FuncUI, 1.5.1"
#r "nuget: Avalonia.Fonts.Inter, 11.2.3"

open System
open MKXK
open ExprTree
open RendererFS
open SkiaSharp


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
let [<Literal>] f2str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
let [<Literal>] f3str = "f(x) = t + k_A - (N * 4.3) / a0 - 8^2"
let [<Literal>] f4str = "f(x) = C_{\\gamma} * C_{\\beta} * C_a"
let [<Literal>] f5str = "f(x) = N_A + 4.5 - C_{\\beta}"
let [<Literal>] f6str = "z(x) = A_n / A_1 - 8^2 + t^2 + a0 - t * C_B / a2 * a1"

let s': Symbols = {
    constants = constants.Keys
    variables = variables.Keys
    functions = []
}

let fns = Map [
    f0str, (FnGeneration.fromTeX s' f0str)
    f1str, (FnGeneration.fromTeX s' f1str)
    f6str, (FnGeneration.fromTeX s' f6str)
]

let maps: Maps = {
    constants = constants
    variables = variables
    functions = fns
}

let x = [|for i in 0..40 -> float i|]

// create some lines, points, and surfaces for the SKChart of the Views.view2
let models = [
    "pts0", Models.createRawModel2 "C_A" "f(x)" x [|for i in 0..40 -> float ((i + 4) * (i - 3))|] Colors.Navy 4.2f
    "pts1", Models.createRawModel2 "C_B" "f(x)" x [|for i in 0..40 -> float i|] Colors.DarkCyan 4.2f
    "m0", Models.createRawModel2 "C_a" "f(x)" x [|for i in 0..40 -> float i / 2.0 + 0.3 * (float i)|] Colors.DarkKhaki 4.2f
    "m1", Models.createRawModel2 "C_A" "f(x)" x [|for i in 0..40 -> float i / 2.0 + 1.3 * (float i * 0.33)|] Colors.Purple 4.2f
    "f(x)", Models.createTeXModel maps f0str "C_A" Colors.Brown 3.0f
    "g(x)", Models.createTeXModel maps f1str "C_A" Colors.Green 3.0f
    "g(x)", Models.createTeXModel maps f2str "C_A" Colors.Black 3.0f
    "g(x)", Models.createTeXModel maps f3str "C_A" Colors.Gold 3.0f
    "g(x)", Models.createTeXModel maps f4str "C_A" Colors.Red 3.0f
    "g(x)", Models.createTeXModel maps f5str "C_A" Colors.Orange 3.0f
    "z(x)", Models.createTeXModel maps f6str "C_A" Colors.Blue 4.0f
]

let r = Renderer()
// r.Run(fun _ -> Views.view2(maps, models))
r.RunParallel(fun _ -> Views.view2(maps, models))

Console.ReadKey ()
