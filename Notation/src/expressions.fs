namespace NotationFS
open System
open System.Text

[<Struct>]
type TokenId =
    | None
    | WhiteSpace
    | Ord
    | Op
    | Bin
    | Rel
    | Open
    | Close
    | Punct
    | Inner
    | Acc
    | Rad
    | Vcent
    | GroupedOpen
    | GroupedClose
    | Number
    | Grouped
    | Identifier
    | MathOperator
    | Symbol
    | Space
    | Binary
    | Sub
    | Super
    | Subup
    | Over
    | Under
    | Underover
    | Up
    | Down
    | Downup
    | Unary
    | Scaled
    | Stretchy
    | Array
    | Text
    | Bad
    | Eof


[<Struct>]
type Token(pos: int, str: string, id: TokenId) =
    member _.Pos = pos
    member _.Str = str
    member _.Id = id


// type TexSymbolType =
//     | Ord = 0
//     | Op = 1
//     | Bin = 2
//     | Rel = 3
//     | Open = 4
//     | Close = 5
//     | Pun = 6
//     | Accent = 7

[<Struct>]
type TexSymbolType =
    | Ord 
    | Op 
    | Bin 
    | Rel 
    | Open 
    | Close 
    | Pun 
    | Accent 

type Alignment =
    | Alignleft = 0
    | AlignCenter = 1
    | AlignRight = 2
    | AlignDefault = 3


[<Struct>]
type HBox = {
    x: float32
    y: float32
    w: float32
    h: float32
    s: float32
}


type Expr = 
    | Number of string
    | Grouped of seq<Expr>
    | Identifier of string 
    | MathOperator of string
    | Symbol of TexSymbolType * string
    | Space of string
    | Binary of string * Expr * Expr
    | Sub of Expr * Expr
    | Super of Expr * Expr
    | Subup of Expr * Expr * Expr
    | Over of Expr * Expr
    | Under of Expr * Expr
    | Underover of Expr * Expr * Expr
    | Up of Expr * Expr
    | Down of Expr * Expr
    | Downup of Expr * Expr * Expr
    | Unary of string * Expr
    | Scaled of string * Expr
    | Stretchy of Expr
    | EArray of list<Alignment> * list<ArrayLine>
    | Text of string 
    | RightBrace
    | Eof

    override x.ToString() =
        match x with
        | Number s -> s
        | Grouped _ -> "Grouped:"
        | Identifier s -> s
        | MathOperator s -> s
        | Symbol (t,s) -> $"{string t} {s}"
        | Space s -> s
        | Binary (s,l,r) -> s
        | Sub _ -> "Sub:"
        | Super _ -> "Super:"
        | Subup _ -> "Subup:"
        | Over _ -> "Over:"
        | Under _ -> "Under:"
        | Underover _ -> "Underover:"
        | Up _ -> "Up:"
        | Down _ -> "Down:"
        | Downup _ -> "Downup:"
        | Unary (s,e) -> s
        | Scaled (s,e) -> s
        | Stretchy _ -> "Stretchy:"
        | EArray _ -> "EArray:"
        | Text s -> s
        | RightBrace -> "}"
        | Eof -> "\Eof" 
        

and ArrayLine = list<list<Expr>>

    // and type ArrayLine = list<list<Expr>>

module Notation =
    let textOps = Map [
        "\\textrm", Text "normal"
        "\\mathrm", Text "normal"
        "\\text",   Text "normal"
        "\\mbox",   Text "normal"
        "\\mathbf", Text "bold"
        "\\textbf", Text "bold"
        "\\mathit", Text "italic"
        "\\textit", Text "italic"
        "\\mathtt", Text "monospace"
        "\\texttt", Text "monospace"
        "\\mathsf", Text "sans-serif"
    ]

    let diacriticals = Map [
        "\\acute", fun e -> Over (e, (Symbol (Accent, "\x00B4")))
        "\\grave", fun e -> Over (e, (Symbol (Accent, "\x0060")))
        "\\breve", fun e -> Over (e, (Symbol (Accent, "\x02D8")))
        "\\check", fun e -> Over (e, (Symbol (Accent, "\x02C7")))
        "\\dot",   fun e -> Over (e, (Symbol (Accent, ".")))
        "\\ddot",  fun e -> Over (e, (Symbol (Accent, "..")))
        "\\vec",   fun e -> Over (e, (Symbol (Accent, "\x20D7")))
        "\\hat",   fun e -> Over (e, (Symbol (Accent, "\x005E")))
        "\\bar",   fun e -> Over (e, (Symbol (Accent, "\x203E")))
        "\\widehat", fun e -> Over (e, (Symbol (Accent, "\x0302")))
        "\\tilde",   fun e -> Over (e, (Symbol (Accent, "~")))
        "\\mathring",  fun e -> Over (e, (Symbol (Accent, "\x00B0")))
        "\\widetilde", fun e -> Over (e, (Symbol (Accent, "\x02DC")))
        "\\overbrace", fun e -> Over (e, (Symbol (Accent, "\xFE37")))
        "\\overrightarrow", fun e -> Over (e, (Symbol (Accent, "\x20D7")))
        "\\overleftarrow",  fun e -> Over (e, (Symbol (Accent, "\x20D6")))
        "\\overbracket",  fun e -> Over  (e, (Symbol (Accent, "\x23B4")))
        "\\overline",     fun e -> Over  (e, (Symbol (Accent, "\x00AF")))
        "\\underbrace",   fun e -> Under (e, (Symbol (Accent, "\xFE38")))
        "\\underbracket", fun e -> Under (e, (Symbol (Accent, "\x23B5")))
        "\\underline",    fun e -> Under (e, (Symbol (Accent, "\x00AF")))
    ]

    let binaryops = ["\\frac"; "\\tfrac"; "\\dfrac"; "\\stackrel"; "\\overset"; "\\underset"; "\\binom"]
    
    let scalers = Map [
        "\\bigg", 2.2
        "\\Bigg", 2.9
        "\\big", 1.2
        "\\Big", 1.6
        "\\biggr", 2.2
        "\\Biggr", 2.9
        "\\bigr", 1.2
        "\\Bigr", 1.6
        "\\biggl", 2.2
        "\\Biggl", 2.9
        "\\bigl", 1.2
        "\\Bigl", 1.6
    ]
    
    let enclosures = Map [ 
        "(", Symbol (Open, "(")
        ")", Symbol (Close, ")")
        "[", Symbol (Open, "[")
        "]", Symbol (Close, "]")
        "\\{", Symbol (Open, "{")
        "\\}", Symbol (Close, "}")
        "\\lbrack", Symbol (Open, "[")
        "\\lbrace", Symbol (Open, "{")
        "\\rbrack", Symbol (Close, "]")
        "\\rbrace", Symbol (Close, "}")
        "\\llbracket", Symbol (Open, "\x27E6")
        "\\rrbracket", Symbol (Close, "\x230B")
        "\\langle", Symbol (Open, "\x27E8")
        "\\rangle", Symbol (Close, "\x27E9")
        "\\lfloor", Symbol (Open, "\x230A")
        "\\rfloor", Symbol (Close, "\x230B")
        "\\lceil", Symbol (Open, "\x2308")
        "\\rceil", Symbol (Close, "\x2309")
        "|", Symbol (Open, "\x2223")
        "|", Symbol (Close, "\x2223")
        "\\|", Symbol (Open, "\x2225")
        "\\|", Symbol (Close, "\x2225")
        "\\vert", Symbol (Open, "\x2223")
        "\\vert", Symbol (Close, "\x2223")
        "\\Vert", Symbol (Open, "\x2225")
        "\\Vert", Symbol (Close, "\x2225")
    ]


    let symbols = Map [
        "+", Symbol (Bin, "+")
        "-", Symbol (Bin, "-")
        "*", Symbol (Bin, "*")
        ",", Symbol (Pun, ",")
        ".", Symbol (Pun, ".")
        ";", Symbol (Pun, ";")
        ":", Symbol (Pun, ":")
        "?", Symbol (Pun, "?")
        ">", Symbol (Rel, ">")
        "<", Symbol (Rel, "<")
        "!", Symbol (Ord, "!")
        "'", Symbol (Ord, "\x02B9")
        "''", Symbol (Ord, "\x02BA")
        "'''", Symbol (Ord, "\x2034")
        "''''", Symbol (Ord, "\x2057")
        "=", Symbol (Rel, "=")
        ":=", Symbol (Rel, ":=")
        "\\mid", Symbol (Bin, "\x2223")
        "\\parallel", Symbol (Rel, "\x2225")
        "\\backslash", Symbol (Bin, "\x2216")
        "/", Symbol (Bin, "/")
        "\\setminus", Symbol (Bin, "\\")
        "\\times", Symbol (Bin, "\x00D7")
        "\\alpha", Symbol (Ord, "\x03B1")
        "\\beta",  Symbol (Ord, "\x03B2")
        "\\chi",   Symbol (Ord, "\x03C7")
        "\\delta", Symbol (Ord, "\x03B4")
        "\\Delta", Symbol (Op, "\x0394")
        "\\epsilon", Symbol (Ord, "\x03B5")
        "\\varepsilon", Symbol (Ord, "\x025B")
        "\\eta", Symbol (Ord, "\x03B7")
        "\\gamma", Symbol (Ord, "\x03B3")
        "\\Gamma", Symbol (Op, "\x0393") 
        "\\iota",  Symbol (Ord, "\x03B9")
        "\\kappa", Symbol (Ord, "\x03BA")
        "\\omega", Symbol (Ord, "\x03C9")
        "\\Omega", Symbol (Op, "\x03A9")
        "\\lambda", Symbol (Ord, "\x03BB")
        "\\Lambda", Symbol (Op, "\x039B") 
        "\\varphi", Symbol (Ord, "\x03D5")
        "\\mu",  Symbol (Ord, "\x03BC")
        "\\nu",  Symbol (Ord, "\x03BD")
        "\\phi", Symbol (Ord, "\x03C6")
        "\\Phi", Symbol (Op, "\x03A6") 
        "\\pi",  Symbol (Ord, "\x03C0")
        "\\Pi",  Symbol (Op, "\x03A0") 
        "\\psi", Symbol (Ord, "\x03C8")
        "\\Psi", Symbol (Ord, "\x03A8")
        "\\rho", Symbol (Ord, "\x03C1")
        "\\sigma", Symbol (Ord, "\x03C3")
        "\\Sigma", Symbol (Op, "\x03A3") 
        "\\theta", Symbol (Ord, "\x03B8")
        "\\tau", Symbol (Ord, "\x03C4")
        "\\vartheta", Symbol (Ord, "\x03D1")
        "\\Theta", Symbol (Op, "\x0398") 
        "\\upsilon", Symbol (Ord, "\x03C5")
        "\\xi", Symbol (Ord, "\x03BE")
        "\\Xi", Symbol (Op, "\x039E") 
        "\\zeta", Symbol (Ord, "\x03B6")
        "\\frac12", Symbol (Ord, "\x00BD")
        "\\frac14", Symbol (Ord, "\x00BC")
        "\\frac34", Symbol (Ord, "\x00BE")
        "\\frac13", Symbol (Ord, "\x2153")
        "\\frac23", Symbol (Ord, "\x2154")
        "\\frac15", Symbol (Ord, "\x2155")
        "\\frac25", Symbol (Ord, "\x2156")
        "\\frac35", Symbol (Ord, "\x2157")
        "\\frac45", Symbol (Ord, "\x2158")
        "\\frac16", Symbol (Ord, "\x2159")
        "\\frac56", Symbol (Ord, "\x215A")
        "\\frac18", Symbol (Ord, "\x215B")
        "\\frac38", Symbol (Ord, "\x215C")
        "\\frac58", Symbol (Ord, "\x215D")
        "\\frac78", Symbol (Ord, "\x215E")
        "\\pm", Symbol (Bin, "\x00B1")
        "\\mp", Symbol (Bin, "\x2213")
        "\\triangleleft", Symbol (Bin, "\x22B2")
        "\\triangleright", Symbol (Bin, "\x22B3")
        "\\cdot", Symbol (Bin, "\x22C5")
        "\\star", Symbol (Bin, "\x22C6")
        "\\ast", Symbol (Bin, "\x002A")
        "\\times", Symbol (Bin, "\x00D7")
        "\\div", Symbol (Bin, "\x00F7")
        "\\circ", Symbol (Bin, "\x2218")
        "\\bullet", Symbol (Bin, "\x2022")
        "\\oplus", Symbol (Bin, "\x2295")
        "\\ominus", Symbol (Bin, "\x2296")
        "\\otimes", Symbol (Bin, "\x2297")
        "\\bigcirc", Symbol (Bin, "\x25CB")
        "\\oslash", Symbol (Bin, "\x2298")
        "\\odot", Symbol (Bin, "\x2299")
        "\\land", Symbol (Bin, "\x2227")
        "\\wedge", Symbol (Bin, "\x2227")
        "\\lor", Symbol (Bin, "\x2228")
        "\\vee", Symbol (Bin, "\x2228")
        "\\cap", Symbol (Bin, "\x2229")
        "\\cup", Symbol (Bin, "\x222A")
        "\\sqcap", Symbol (Bin, "\x2293")
        "\\sqcup", Symbol (Bin, "\x2294")
        "\\uplus", Symbol (Bin, "\x228E")
        "\\amalg", Symbol (Bin, "\x2210")
        "\\bigtriangleup", Symbol (Bin, "\x25B3")
        "\\bigtriangledown", Symbol (Bin, "\x25BD")
        "\\dag", Symbol (Bin, "\x2020")
        "\\lhd", Symbol (Bin, "\x22B2")
        "\\rhd", Symbol (Bin, "\x22B3")
        "\\neq", Symbol (Rel, "\x2260")
        "\\leq", Symbol (Rel, "\x2264")
        "\\dagger",  Symbol (Bin, "\x2020")
        "\\ddagger", Symbol (Bin, "\x2021")
        "\\unlhd", Symbol (Bin, "\x22B4")
        "\\unrhd", Symbol (Bin, "\x22B5")
        "\\ddag",  Symbol (Bin, "\x2021")
        "\\lt", Symbol (Rel, "<")
        "\\gt", Symbol (Rel, ">")
        "\\ne", Symbol (Rel, "\x2260")
        "\\le", Symbol (Rel, "\x2264")
        "\\leqslant", Symbol (Rel, "\x2264")
        "\\ge", Symbol (Rel, "\x2265")
        "\\geq", Symbol (Rel, "\x2265")
        "\\geqslant", Symbol (Rel, "\x2265")
        "\\ll", Symbol (Rel, "\x226A")
        "\\gg", Symbol (Rel, "\x226B")
        "\\doteq", Symbol (Rel, "\x2250")
        "\\equiv", Symbol (Rel, "\x2261")
        "\\prec",  Symbol (Rel, "\x227A")
        "\\succ",  Symbol (Rel, "\x227B")
        "\\preceq", Symbol (Rel, "\x227C")
        "\\succeq", Symbol (Rel, "\x227D")
        "\\subset", Symbol (Rel, "\x2282")
        "\\supset", Symbol (Rel, "\x2283")
        "\\subseteq", Symbol (Rel, "\x2286")
        "\\supseteq", Symbol (Rel, "\x2287")
        "\\sqsubset", Symbol (Rel, "\x228F")
        "\\sqsupset", Symbol (Rel, "\x2290")
        "\\sqsubseteq", Symbol (Rel, "\x2291")
        "\\sqsupseteq", Symbol (Rel, "\x2292")
        "\\sim",   Symbol (Rel, "\x223C")
        "\\simeq", Symbol (Rel, "\x2243")
        "\\approx", Symbol (Rel, "\x2248")
        "\\bowtie", Symbol (Rel, "\x22C8")
        "\\models", Symbol (Rel, "\x22A8")
        "\\propto", Symbol (Rel, "\x221D")
        "\\in", Symbol (Rel, "\x2208")
        "\\ni", Symbol (Rel, "\x220B")
        "\\owns",  Symbol (Rel, "\x220B")
        "\\vdash", Symbol (Rel, "\x22A2")
        "\\cong",  Symbol (Rel, "\x2245")
        "\\Join",  Symbol (Rel, "\x22C8")
        "\\dashv", Symbol (Rel, "\x22A3")
        "\\perp",  Symbol (Rel, "\x22A5")
        "\\smile", Symbol (Rel, "\x2323")
        "\\frown", Symbol (Rel, "\x2322")
        "\\asymp", Symbol (Rel, "\x224D")
        "\\notin", Symbol (Rel, "\x2209")
        "\\gets",  Symbol (Rel, "\x2190")
        "\\leftarrow", Symbol (Rel, "\x2190")
        "\\to", Symbol (Rel, "\x2192")
        "\\rightarrow", Symbol (Rel, "\x2192")
        "\\leftrightarrow", Symbol (Rel, "\x2194")
        "\\uparrow", Symbol (Rel, "\x2191")
        "\\downarrow", Symbol (Rel, "\x2193")
        "\\updownarrow", Symbol (Rel, "\x2195")
        "\\Leftarrow", Symbol (Rel, "\x21D0")
        "\\Rightarrow", Symbol (Rel, "\x21D2")
        "\\Leftrightarrow", Symbol (Rel, "\x21D4")
        "\\iff", Symbol (Rel, "\x21D4")
        "\\Uparrow", Symbol (Rel, "\x21D1")
        "\\Downarrow", Symbol (Rel, "\x21D3")
        "\\Updownarrow", Symbol (Rel, "\x21D5")
        "\\mapsto", Symbol (Rel, "\x21A6")
        "\\longleftarrow", Symbol (Rel, "\x2190")
        "\\longrightarrow", Symbol (Rel, "\x2192")
        "\\longleftrightarrow", Symbol (Rel, "\x2194")
        "\\Longleftarrow", Symbol (Rel, "\x21D0")
        "\\Longrightarrow", Symbol (Rel, "\x21D2")
        "\\Longleftrightarrow", Symbol (Rel, "\x21D4")
        "\\longmapsto", Symbol (Rel, "\x21A6")
        "\\sum", Symbol (Op, "\x2211")
        "\\prod", Symbol (Op, "\x220F")
        "\\bigcap", Symbol (Op, "\x22C2")
        "\\bigcup", Symbol (Op, "\x22C3")
        "\\bigvee", Symbol (Op, "\x22C1")
        "\\coprod", Symbol (Op, "\x2210")
        "\\bigwedge", Symbol (Op, "\x22C0")
        "\\bigsqcap", Symbol (Op, "\x2A05")
        "\\bigsqcup", Symbol (Op, "\x2A06")
        "\\bigoplus", Symbol (Op, "\x2A01")
        "\\biguplus", Symbol (Op, "\x2A04")
        "\\bigodot",  Symbol (Op, "\x2A00")
        "\\int", Symbol (Op, "\x222B")
        "\\bigotimes", Symbol (Op, "\x2A02")
        "\\iint", Symbol (Op, "\x222C")
        "\\oint", Symbol (Op, "\x222E")
        "\\dots", Symbol (Ord, "\x2026")
        "\\iiint", Symbol (Op, "\x222D")
        "\\prime", Symbol (Ord, "\x2032")
        "\\ldots", Symbol (Ord, "\x2026")
        "\\cdots", Symbol (Ord, "\x22EF")
        "\\vdots", Symbol (Ord, "\x22EE")
        "\\ddots", Symbol (Ord, "\x22F1")
        "\\forall", Symbol (Op, "\x2200")
        "\\exists", Symbol (Op, "\x2203")
        "\\Re", Symbol (Ord, "\x211C")
        "\\Im", Symbol (Ord, "\x2111")
        "\\wp", Symbol (Ord, "\x2118")
        "\\aleph", Symbol (Ord, "\x2135")
        "\\infty", Symbol (Ord, "\x221E")
        "\\hbar", Symbol (Ord, "\x210F")
        "\\ell", Symbol (Ord, "\x2113")
        "\\emptyset", Symbol (Ord, "\x2205")
        "\\partial", Symbol (Ord, "\x2202")
        "\\nabla", Symbol (Ord, "\x2207")
        "\\angle", Symbol (Ord, "\x2220")
        "\\diamond", Symbol (Op, "\x22C4")
        "\\triangle", Symbol (Ord, "\x25B3")
        "\\therefore", Symbol (Pun, "\x2234")
        "\\Diamond", Symbol (Op, "\x25C7")
        "\\neg", Symbol (Op, "\x00AC")
        "\\lnot", Symbol (Ord, "\x00AC")
        "\\bot", Symbol (Ord, "\x22A5")
        "\\top", Symbol (Ord, "\x22A4")
        "\\square", Symbol (Ord, "\x25AB")
        "\\Box", Symbol (Op, "\x25A1")
        "\\wr", Symbol (Ord, "\x2240")
        "\\!", Space "-0.167em"
        "\\,", Space "0.167em"
        "\\>", Space "0.222em"
        "\\:", Space "0.222em"
        "\\;", Space "0.278em"
        "~", Space "0.333em"
        "\\quad", Space "1em"
        "\\qquad", Space "2em"
        "\\arccos", MathOperator "arccos"
        "\\arcsin", MathOperator "arcsin"
        "\\arctan", MathOperator "arctan"
        "\\arg",  MathOperator "arg"
        "\\cos",  MathOperator "cos"
        "\\cosh", MathOperator "cosh"
        "\\cot",  MathOperator "cot"
        "\\coth", MathOperator "coth"
        "\\csc", MathOperator "csc"
        "\\deg", MathOperator "deg"
        "\\det", MathOperator "det"
        "\\dim", MathOperator "dim"
        "\\exp", MathOperator "exp"
        "\\gcd", MathOperator "gcd"
        "\\hom", MathOperator "hom"
        "\\inf", MathOperator "inf"
        "\\ker", MathOperator "ker"
        "\\liminf", MathOperator "liminf"
        "\\limsup", MathOperator "limsup"
        "\\lim", MathOperator "lim"
        "\\lg",  MathOperator "lg"
        "\\ln",  MathOperator "ln"
        "\\log", MathOperator "log"
        "\\max", MathOperator "max"
        "\\min", MathOperator "min"
        "\\Pr",  MathOperator "Pr"
        "\\sec", MathOperator "sec"
        "\\sin", MathOperator "sin"
        "\\sinh", MathOperator "sinh"
        "\\sup",  MathOperator "sup"
        "\\tan",  MathOperator "tan"
        "\\tanh", MathOperator "tanh"
    ] 
