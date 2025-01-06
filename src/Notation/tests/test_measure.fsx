#r "../bin/Debug/net8.0/Notation.dll"

open Notation

let functions = [
    "g(x) = \\int_a^b \\frac{1}{2} x^2 dx"
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
    @"f(x) = \frac{a2}{C_B ^ {E_A}} \cdot (\frac{-\frac{(C_A)}{\sqrt{T}}}{(E_A) * (N_A) / \frac{-C_{\beta} \cdot (67,113.009)}{A_2}} + T \cdot 56,751.623)"
    @"f(x) = \frac{C_B}{\sqrt{z(x)}} ^ {\log{49,028.332 \cdot (C_A)}}"
    @"f(x) = \exp(\frac{z(x)}{+T}) / \frac{(f(x)) * t + C_A / \frac{\sqrt{\frac{A_2}{\frac{50,859.769}{N}}}}{a0} * a1}{94,961.952} + 41,255.712"
    @"f(x) = \ln{90,408.433} / \exp(\frac{\sqrt{46,160.067}}{\exp(\frac{C_A}{\frac{(a3)}{A_n}})}) ^ {23,103.608}"
    @"f(x) = \frac{C_A * \frac{\sqrt{\frac{\exp(\ln{+\frac{A_1}{A_0}})}{k_A}}}{\exp(t)} ^ {(a3)}}{C_{\gamma}} + a0"
    @"(x) = C_A \cdot \frac{R}{\frac{g(x)}{(\log{99,767.598})} * C_{AB} / \frac{N_A}{\frac{-z(x)}{A_0 \cdot 53,250.203}}} / (1,504.787)"
]

for fn in (List.map Parser.parseExprs functions) do
    let size = Typesetting.Measure.totalSize fn
    printfn "%A" size
    
