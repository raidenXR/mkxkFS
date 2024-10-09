#load "../src/serializers.fs"

open MKXK
open Html

let h = ["col0"; "col1"; "col2"]

let tt = [
    [1593; 2; 3]
    [1; 2; 333]
    [1; 266; 3]
    [1; 2; 34]
    [1; 2; 3]
    [166; 2; 3550]
]

let fns = ["$f(x) = \\int_a^b x$"; "$g(x) = f(x) - \\frac{A}{B}$"]
let t = ["f0"; "f1"; "f2"]


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
|> table (Some "omg caption!!!") (Some h) (Some t) tt
|> header 3 "image display"
|> image "images/image3.png"
|> close "tests_output/html.html"

// openbrowser "htmltest.html"



open Ascii
let ascii = AsciiBuilder()
ascii
|> header "title 1: ordered list"
|> olist [1;3;4;5;6;7;7]
|> header "title 2: unordered list"
|> ulist [1;3;4;5;6;7;7]
|> header "table title display"
|> line "some description for the following table"
|> line "spawning in multiple lines"
|> table ["col0"; "col1"; "col2"] tt
|> close "tests_output/ascii.txt"



