
#### MKXK

MKXK is a library for parsing LaTeX mathematical formulas, creating ASTs and evaluating those trees.   

First thing for the parser and/or FnGenerator to work, symbols must be defined.   
Symbols are composed of three maps. Constant map, Variables map, Functions map.


First define a table of constants and their values:

```
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
```

and a table of variables:
```

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
```

Define some string literals of TeX formulas, that are compounds of the previously   
defined constants and variables and parse those string into ASTs.

```
// define some tex strings
let [<Literal>] f0str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
let [<Literal>] f1str = "g(x) = T + k_A - (C_A * 4.3) / C_B - t^2"
let [<Literal>] f2str = "z(x) = A_n + A_2 - (C_A * 4.3) / A_1 - 8^2 + t^2"

let s': Symbols = {
    constants = cons.Keys 
    variables = List.map (fun (s,v) -> s) vars
    functions = []
}

let fn0 = (FnGeneration.fromTeX s' f0str)
let fn1 = (FnGeneration.fromTeX s' f1str)
let fn2 = (FnGeneration.fromTeX s' f2str) 

// combine constants, variables and functions into one record.
let maps: Maps = {constants = cons; variables = Map.ofList vars; functions = Map []}

// create random generated function with the defined nodes as symbols
let rand_fn = FnGeneration.generatefn "f(x)" s' TokenIdProbs.Default 0.91 103.45 40
```
**P.S** you can check `src/FnGenerator.fs` for more details about the implementation  
or modify it, to suit better your specific use case.

The `Evaluation` module contains functions for enumerating the ASTs.

on top of that is implements some utility classes, such as HtmlBuilder and Gnuplot.

HtmlBuilder class for creating simple html files, as a file format for supporting   
text, list, headers tables, formulas and images.    

```
let html = HtmlBuilder()
html 
|> header 2 "some header"
|> line "some line in the doc"
|> header 4 "ordered list"
|> olist [1;3;4;5;6;7;7]
|> line "insert some ordered list of functions"
|> olist fns
|> header 4 "unordered list"
|> ulist [1;3;4;5;6;7;7]
|> header 4 "codeblock"
|> code "let b = 0"
|> header 4 "latex function"
|> latexfunctionln "f(x) = A \\cdot b"
|> header 5 "table example"
|> table (Some "omg caption!!!") h t tt
|> header 3 "image display"
|> image "images/image3.png"
|> close "tests_output/html.html"

// openbrowser "htmltest.html"
```

Gnuplot class for seamlessly integrating Gnuplot over F# apps.   
It defines a custom `|>>` operator that appends a string at a StringBuilder.   
As a result, it supports the gnuplot syntax as is.  

More examples can be found over `tests/gnuplot_test.fsx`

```
let gnu2 = Gnuplot("tests_output/images/image2.png")
gnu2
|>> "set terminal pngcairo  transparent enhanced font 'arial,10' fontscale 1.0 size 600, 400" 
|>> "set label 1 'plot for [n=2:10] sin(x*n)/n' at graph 0.95, 0.92, 0 right norotate back nopoint"
|>> "set style data lines"
|>> "set title 'Iteration within plot command'" 
|>> "set xrange [ 0.00000 : 3.00000 ] noreverse nowriteback"
|>> "set x2range [ * : * ] noreverse writeback"
|>> "set yrange [ * : * ] noreverse writeback"
|>> "set y2range [ * : * ] noreverse writeback"
|>> "set zrange [ * : * ] noreverse writeback"
|>> "set cbrange [ * : * ] noreverse writeback"
|>> "set rrange [ * : * ] noreverse writeback"
|>> "set colorbox vertical origin screen 0.9, 0.2 size screen 0.05, 0.6 front  noinvert bdefault"
|>> "NO_ANIMATION = 1"
|>> "n = 10"
|>> "plot for [n=2:10] sin(x*n)/n notitle lw (13-n)/2"
|> Gnuplot.run
```

![](../../images/image2.png)



