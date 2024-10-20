#r "../bin/Debug/net8.0/Notation.dll"
// #r "nuget: SkiaSharp, 2.88.6"
// #load "../src/expressions.fs"
// #load "../src/parsing.fs"
// #load "../src/typesetting.fs"

open System
open System.IO
open System.Diagnostics
open NotationFS
// open SkiaSharp

let functions = [
    // "g(x) = \\int_a^b \\frac{1}{2} x^2 dx"
    "f(x) = 4.3 \\cdot x^{3A_z} \\cdot \\Gamma - N_A + \\frac{A + D}{B - G} + (A - B_{\\gamma})"
    "f(x) = \\int _a ^b \\frac{(A + B - 94.3)}{e^{-RT} \\cdot 9.43 x} dx"
    "f(x) = \\int _a ^b \\frac{(A + B - 94.3)}{e^{-RT \\cdot \\frac{K_l}{N_t}} \\cdot 9.43 x} dx"
    "\\frac{(A-B)}{(e^{-RT})} + \\frac{1}{2}"
    "f(x) = (A_n + B_{n + 1}) - \\frac{1}{2} x^2 \\cdot \\gamma"
    "g(x) = E^{-RT} + 4.213 T - 6.422 T - \\gamma^{-2}"
    "z(x) = 3.2343 e^{-1.2} + 8.5"
    "a(x) = \\frac{Z - 9.2 + A^2}{e^{0.8}} + \\frac{x^2 + 2 * x + 1}{x^3 - 1}"
    @"f(x) = \frac{R_{\epsilon} + A_1 - -\frac{\frac{C_{\gamma}}{C_B}}{C_C + \frac{(C_{\alpha}) + C_{\alpha}}{C_a}} - R_{\epsilon}}{C_{\beta} - 3.738} / (A_e)"
    @"f(x) = \frac{\frac{1}{2}}{e + \frac{A}{\frac{(e + 2)}{G - \gamma}}}"
]


// check lexer is working
// for str in functions do
//     Console.WriteLine str
//     let lexer = Lexer(str)
//     for token in lexer.tokens() do
//         if token.Id = TokenId.Bad || token.Id = TokenId.WhiteSpace then ignore ()
//         else Console.WriteLine("{0}, {1}", token.Str, token.Id)
//     Console.WriteLine "\n"
    

// check parser is working

let mutable i = 0

// Typesetting.WDTypefaces()

for str in functions do
    Console.WriteLine str
    let parser = Parser(str)
    let exprs = parser.exprs()
    // use paint = new SKPaint(
    //     Color = SKColors.Black,
    //     IsAntialias = true,
    //     TextSize = 16f,
    //     StrokeWidth = 2.0f
    //     )
    // let size = Typesetting.measure exprs paint 1.0f
    // printfn "%A" size
    use fs = File.Create($"notation_images/fn{i}.png")
    Typesetting.render exprs fs
    i <- i + 1
    fs.Flush()
    fs.Close()
    
// let [<Literal>] dir_name = "notation_images"
// Directory.CreateDirectory dir_name |> ignore

// let ms = new MemoryStream(8 * 1024)
// let dt0 = Stopwatch.GetTimestamp()
// let mutable i = 0
