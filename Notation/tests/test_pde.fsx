#r "../bin/Debug/net8.0/Notation.dll"
#load "../../MKXK/src/serializers.fs"

open System
open System.IO
open System.Diagnostics
open NotationFS

let functions = [
    @"f(x) = \frac{A}{B}"
    @"f(x) = \pde{A}{B}"
    @"f(x) = \ode{A}{B}"
]

let [<Literal>] dir_name = "notation_images2"
Directory.CreateDirectory dir_name |> ignore

let mutable i = 1

for fn in (List.map NotationFS.Parser.parseExprs functions) do    
    let hbox = Typesetting.Measure.totalSize fn 
    use fs = File.Create($"notation_images2/fn{i}.png")
    Typesetting.render false fs fn
    i <- i + 1
    fs.Flush()
    fs.Close()
