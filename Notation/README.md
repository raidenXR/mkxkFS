example how to use it, and create an .png image.
```
open System
open System.IO
open NotationFS

let tex = @"f(x) = \frac{(C_{\beta}) - C_B}{(f(x)) / (\frac{C_A}{(N)}) \cdot (266.955 - +(C_a) * \exp(R / \sqrt{(\ln{C_{AB}} * \frac{\frac{((N_A))}{C_a}}{k_A}) + A_1}))}"
let exprs = Parser.parseExprs tex
let fs = File.Create("sample.png")
Typesetting.render fs exprs
fs.Flush()
fs.Close()
```
