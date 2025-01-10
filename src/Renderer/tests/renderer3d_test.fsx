#r "nuget: System.Drawing.Common, 8.0.0"
#r "../bin/Debug/net8.0/mkxkFS.dll"
#r "../bin/Debug/net8.0/Renderer.dll"
#r "../bin/Debug/net8.0/SKCharts.dll"

#r "nuget: Avalonia, 11.0.6"
#r "nuget: Avalonia.Desktop, 11.0.6"
#r "nuget: Avalonia.Themes.Fluent, 11.0.6"
#r "nuget: Avalonia.FuncUI, 1.1.0"
#r "nuget: Avalonia.Fonts.Inter, 11.0.6"

open Avalonia
open Avalonia.Controls.Primitives
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout

open System
open MKXK
open ExprTree
open SkiaSharp
open SKCharts
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

// some simple function to use as example for the surface
let [<Literal>] fx = @"f(x,y) = x^2 + 2 \cdot y - 7 * x * T / y + C_a * C_B"

let s': Symbols = {
    constants = constants.Keys
    variables = variables.Keys
    functions = []
}

let maps: Maps = {
    constants = constants
    variables = variables
    functions = Map[]
}

let x = Array.zeroCreate<float> 930
let y = Array.zeroCreate<float> 930
let z = Array.zeroCreate<float> 930

let f = Binder.bind (Parser.parse s' fx)

// fill the arrays with values
for i in 0..30 do
    for j in 0..30 do
        x[i * 30 + j] <- i
        y[i * 30 + j] <- j
        maps.variables["x"].V <- ValueSome i
        maps.variables["y"].V <- ValueSome j
        z[i * 30 + j] <- Evaluation.eval maps "x" f   

let models = [Model3.create ChartType.Surface x y z 30 30 SKColors.Black]



let view3 () = Component(fun ctx ->
    DockPanel.create [
        DockPanel.lastChildFill true
        DockPanel.children [
            StackPanel.create [
                StackPanel.dock Dock.Bottom
                StackPanel.children [
                
                ]
            ]            
        ]
    ]
)



let r = Renderer()
r.RunParallel(fun _ -> view3())
