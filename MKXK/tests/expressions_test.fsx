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
let vs = ExprTree.variables f6
Console.WriteLine("\n--binaries len: {0}", (Seq.length bs))
Console.WriteLine("--constants len: {0}", (Seq.length cs))
Console.WriteLine("--variables len: {0}", (Seq.length vs))
Console.WriteLine ""


// check that mutation via ref cells is working
// check that binder and evaluation work
let f6_before = copy f6
let (_, _, op) = bs[1]
op.Value <- Cdot
cs[0].Value <- "k_A"
vs[1].Value <- "C_A"
let res6_before = Evaluation.eval cons vars (Map []) "f(x)" (Binder.bind f6_before) // before mutation
let res6        = Evaluation.eval cons vars (Map []) "f(x)" (Binder.bind f6) // after mutation
printfn "\n--test mutation"
printfn "f6 before: eval: %g \n - %s" res6_before (latex f6_before)
printfn "f6 after : eval: %g \n - %s" res6 (latex f6)
printfn "\n"
for (i, j) in (compare f6_before f6) do printfn "%s , %s" (string i) (string j)


// check that random fn generation works
let mutable f2 = FnGeneration.generatefn "f(x)" cons.Keys vars.Keys [] 0.91 103.45 40
while (ExprTree.variables f2).Length < 2 do 
    // ensure that random generated fn has at least one variable (besides f(x))
    f2 <- FnGeneration.generatefn "f(x)" cons.Keys vars.Keys [] 0.91 103.45 40
((ExprTree.variables f2)[1]).Value <- "C_A"   // ensure that specific variable-target exists
printfn "\n--random generated fn"
Console.WriteLine(latex f2)

// check the binder and evaluation on ranmdom generated fns
let res2 = Evaluation.eval cons vars (Map []) "C_A" (Binder.bind f2)
Console.WriteLine("\n--test evaluation\neval f2: {0}\n", res2)


// check that optimization works
let xseries = [|for i in 0..100 -> float i|]
let yseries = [|for i in 0..100 -> 0.32 * float i + 34.6|]
let args: Mutation.OptimizationDesc  = {
    constants = cons
    variables = vars
    functions = Map []
    x = xseries
    y = yseries
    err = 10    
}
let fret = Mutation.optimizefn args 100 0.10 1000. "C_A" f2
let res_org = Evaluation.eval cons vars (Map []) "f(x)" (Binder.bind f2)
let res_opt = Evaluation.eval cons vars (Map []) "f(x)" (Binder.bind fret)
printfn "-original  fn  -eval: %g \n %s\n" res_org (latex f2)
printfn "-optimized fn  -eval: %g \n %s\n" res_opt (latex fret)
for (i, j) in (compare f2 fret) do printfn "%s , %s" (string i) (string j)

