#r "../bin/Debug/net8.0/Notation.dll"
#load "../../MKXK/src/serializers.fs"

open System
open System.IO
open System.Diagnostics
open Notation

let functions = [
    @"f(x) = \frac{A}{B}"
    @"f(x) = \pde{A}{B}"
    @"f(x) = \ode{A}{B}"
    @"f(x) = \hat a + \hat{A} - \tilde z / \tilde Z + B"
    @"f(x) = \widehat \frac{A}{B} - \widehat \ode{T}{t} + \nabla T - \pde{A}{t}"
    @"f(x) = A + \ddot b \cdot \ddot{B} - 4 * 3.2"
    @"f(x) = A + B - 4 * 3.2 \cdot \vec \gamma - \vec \Gamma + B"
]

let [<Literal>] dir_name = "notation_images2"
Directory.CreateDirectory dir_name |> ignore

let mutable i = 1

for (i,tex) in (List.indexed functions) do    
    use fs = File.Create($"notation_images2/fn{i + 1}.png")
    tex
    |> Typesetting.parseTeX 
    |> Typesetting.render false fs
