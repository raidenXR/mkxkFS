#load "../src/expressions.fs"
#load "../src/Lexer.fs"
#load "../src/Parser.fs"
#load "../src/FnGenerator.fs"

open System
open MKXK
open ExprTree

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
let [<Literal>] f1str = "f(x) = T + k_A - (C_A * 4.3) / C_B - t^2"
// let [<Literal>] f2str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
// let [<Literal>] f3str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
// let [<Literal>] f4str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
// let [<Literal>] f5str = "f(x) = N_A + 4.5 - (C_A * 4.3) / C_B - 8^2"
let [<Literal>] f6str = "f(x) = A_n + A_2 - (C_A * 4.3) / A_1 - 8^2 + t^2"

// parse some tex strings
let f0 = Parser.parse f0str cons.Keys vars.Keys []
let f1 = Parser.parse f1str cons.Keys vars.Keys []
let f6 = Parser.parse f6str cons.Keys vars.Keys []

// check that tree and count functions are working
Console.WriteLine(tree f6)
Console.WriteLine(count f6)

// check that nodes function is working
(nodes f6) |> Seq.iter (fun t -> Console.WriteLine (string t))

// check that binaties function is working
let bs = ExprTree.binaries f6
let cs = ExprTree.constants f6
Console.WriteLine("binaries len: {0}", (Seq.length bs))
Console.WriteLine("binaries len: {0}", (Seq.length cs))

// check that mutation via ref cells is working
Console.WriteLine("f6 before mutuation\n {0}", (latex f6))
let (_, _, op) = bs[1]
op.Value <- Cdot
let c = cs[0]
c.Value <- "k_A"
Console.WriteLine("f6 after mutation\n {0}", (latex f6))

// check that binder and evaluation work
let bf6 = Binder.bind f6
let res6 = Evaluation.eval cons vars (Map []) "f(x)" bf6 
Console.WriteLine("eval f6: {0}", res6)

// check that random fn generation works
let f2 = FnGeneration.generatefn "f(x)" cons.Keys vars.Keys [] 0.91 103.45 40
Console.WriteLine(latex f2)

// check the binder and evaluation on ranmdom generated fns
let bf2 = Binder.bind f2
let res2 = Evaluation.eval cons vars (Map []) "f(x)" bf2
Console.WriteLine("eval f2: {0}", res2)

// let f3 = FnGeneration.generateFn "f(x)" cons.Keys vars.Keys [] 0.91 103.45
// let f4 = FnGeneration.generateFn "f(x)" cons.Keys vars.Keys [] 0.91 103.45
// let f5 = FnGeneration.generateFn "f(x)" cons.Keys vars.Keys [] 0.91 103.45

// let fns = [f0; f1; f2; f3; f4; f5; f5; f6]


