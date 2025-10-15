#r "nuget: System.Drawing.Common, 8.0.0"
#r "../bin/Release/net8.0/mkxkFS.dll"
#r "../bin/Release/net8.0/Notation.dll"
#r "../bin/Release/net8.0/mkxk-viewer.dll"
#r "../bin/Release/net8.0/SKCharts.dll"
#r "../bin/Release/net8.0/SKCharts.Avalonia.dll"

#r "nuget: Avalonia, 11.2.3"
#r "nuget: Avalonia.Desktop, 11.2.3"
#r "nuget: Avalonia.Themes.Fluent, 11.2.3"
#r "nuget: Avalonia.FuncUI, 1.5.1"
#r "nuget: Avalonia.Fonts.Inter, 11.2.3"


open System
open System.IO
open MKXK
open ExprTree
open SKCharts
open SKCharts.Avalonia
open MKXK.Viewer


// define some tex strings
let [<Literal>] f0str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
let [<Literal>] f1str = "g(x) = T + k_A - (C_A * 4.3) / C_B - t^2"
let [<Literal>] f6str = "z(x) = A_n + A_2 - (C_A * 4.3) / A_1 - 8^2 + t^2 + g(x)"

// raw data points
let x = Array.init 40 (fun i -> float i)
let y = Array.init 40 (fun i -> 1e-3 * x[i] * x[i] - 0.3 * x[i] + 1.25)
let z = Array.init 40 (fun i -> 1e-2 * x[i] * x[i])


// create some table of variables
let vars = Define.variables [
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

// create some table of constants
let cons = Define.constants [
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

// define the symbols of the system
let s' = Define.symbols cons vars Map.empty

// assign some pre-existing functions
let fns = Define.functions s' [f0str; f1str; f6str]

// group constants, variables and functions together as one object
let maps = {constants = cons; variables = vars; functions = fns}

let mutable counter = 1
let args = System.Environment.GetCommandLineArgs()
let N = if args.Length < 4 then 1000 else Int32.Parse (args[2])  // optimization loop
let L = if args.Length < 4 then 300 else Int32.Parse (args[3])   // no of rng fns

// make a function with print utilities to run in parallel
let parallel_opt = 
    let desc: FnMutation.Params = {maps = maps; x = x; y = z; err = 1e-2}
    let cout (pair:Expr * float) =
        if counter % 100 = 0 then
            let p = 100. * (float counter / float L)
            Console.SetCursorPosition(0,10)
            Console.WriteLine ($"fn optimized: {p:N2}" + "%")
        counter <- counter + 1
        pair
    (FnMutation.optimizefn desc N 0.1 1e3 "C_A") >> cout
    

// execute the algorithm (in parallel) and measure its time
printfn "rng-fn generation #time"
#time
let probs = {FnGeneration.Params.Default with Pfunction = 15; Pnumber = 5; Pfrac = 15}
let pipe0 = Array.Parallel.init L (fun _ -> FnGeneration.generatefn "f(x)" s' probs 0.01 1e5 40)
#time

#time
printfn "\nrng-fn optimization #time"
let (fns_optimized, errs) = 
    pipe0
    |> Array.Parallel.map parallel_opt
    |> Array.sortBy (fun (f,err) -> err)
    |> Array.take 30
    |> Array.map (fun (x,y) -> latex x, y)
    |> Array.unzip
#time


// keep a log of generated fns and their errors as an html file
let html = Html.HtmlBuilder()
html
|> Html.header 2 "constants"
|> Html.table None ["name"; "value"] [] ([for p in cons -> [$"${p.Key}$"; (string p.Value)]])
|> Html.header 2 "variables"
|> Html.table None ["name"; "min"; "max"] [] ([for v in vars -> [$"${v.Key}$"; (string v.Value.A); (string v.Value.B)]])
|> Html.header 2 "functions"
|> Html.ulist (List.map (fun x -> $"${x}$") [f0str; f1str; f6str])
|> Html.header 2 "rng functions"
|> Html.olist (Array.map2 (fun x y -> $"${x}$      err:{y:N6}") fns_optimized[0..9] errs[0..9])
|> Html.line ""
|> Html.line ""
|> Html.olisti 11 (Array.map2 (fun x y -> $"${x}$      err:{y:N6}") fns_optimized[10..] errs[10..])
|> Html.close "functions.html"


let models = [
    "rp0", Models.createRawModel2 "C_A" "C_a" x y Colors.Navy 4.2f
    "rp1", Models.createRawModel2 "C_B" "C_b" x z Colors.Purple 4.2f
    "f1(x)", Models.createTeXModel maps fns_optimized[0] "C_A" Colors.Green 2.0f
    "f2(x)", Models.createTeXModel maps fns_optimized[1] "C_A" Colors.Red 2.0f
    "f3(x)", Models.createTeXModel maps fns_optimized[2] "C_A" Colors.Blue 2.0f
    "f4(x)", Models.createTeXModel maps fns_optimized[3] "C_A" Colors.Brown 2.0f
    "f5(x)", Models.createTeXModel maps fns_optimized[4] "C_A" Colors.Silver 2.0f
    "f6(x)", Models.createTeXModel maps fns_optimized[5] "C_A" Colors.Black 2.0f
    "f7(x)", Models.createTeXModel maps fns_optimized[6] "C_A" Colors.Olive 2.0f
    "f8(x)", Models.createTeXModel maps fns_optimized[7] "C_A" Colors.OrangeRed 2.0f
    "f9(x)", Models.createTeXModel maps fns_optimized[8] "C_A" Colors.CornflowerBlue 2.0f
    "f10(x)", Models.createTeXModel maps fns_optimized[9] "C_A" Colors.Fuchsia 2.0f
    "f11(x)", Models.createTeXModel maps fns_optimized[10] "C_A" Colors.DarkMagenta 2.0f
]

let renderer = Renderer("MKXK-Viewer")
renderer.RunParallel(fun _ -> Views.view2(maps, models))
Console.ReadKey()
