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
    | ExprNone
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
        | ExprNone -> "None"
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
        "\\acute", fun e -> Over (e, (Symbol (Accent, "\u00B4")))
        "\\grave", fun e -> Over (e, (Symbol (Accent, "\u0060")))
        "\\breve", fun e -> Over (e, (Symbol (Accent, "\u02D8")))
        "\\check", fun e -> Over (e, (Symbol (Accent, "\u02C7")))
        "\\dot",   fun e -> Over (e, (Symbol (Accent, ".")))
        "\\ddot",  fun e -> Over (e, (Symbol (Accent, "..")))
        "\\vec",   fun e -> Over (e, (Symbol (Accent, "\u20D7")))
        "\\hat",   fun e -> Over (e, (Symbol (Accent, "\u005E")))
        "\\bar",   fun e -> Over (e, (Symbol (Accent, "\u203E")))
        "\\widehat", fun e -> Over (e, (Symbol (Accent, "\u0302")))
        "\\tilde",   fun e -> Over (e, (Symbol (Accent, "~")))
        "\\mathring",  fun e -> Over (e, (Symbol (Accent, "\u00B0")))
        "\\widetilde", fun e -> Over (e, (Symbol (Accent, "\u02DC")))
        "\\overbrace", fun e -> Over (e, (Symbol (Accent, "\uFE37")))
        "\\overrightarrow", fun e -> Over (e, (Symbol (Accent, "\u20D7")))
        "\\overleftarrow",  fun e -> Over (e, (Symbol (Accent, "\u20D6")))
        "\\overbracket",  fun e -> Over  (e, (Symbol (Accent, "\u23B4")))
        "\\overline",     fun e -> Over  (e, (Symbol (Accent, "\u00AF")))
        "\\underbrace",   fun e -> Under (e, (Symbol (Accent, "\uFE38")))
        "\\underbracket", fun e -> Under (e, (Symbol (Accent, "\u23B5")))
        "\\underline",    fun e -> Under (e, (Symbol (Accent, "\u00AF")))
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
        "\\llbracket", Symbol (Open, "\u27E6")
        "\\rrbracket", Symbol (Close, "\u230B")
        "\\langle", Symbol (Open, "\u27E8")
        "\\rangle", Symbol (Close, "\u27E9")
        "\\lfloor", Symbol (Open, "\u230A")
        "\\rfloor", Symbol (Close, "\u230B")
        "\\lceil", Symbol (Open, "\u2308")
        "\\rceil", Symbol (Close, "\u2309")
        "|", Symbol (Open, "\u2223")
        "|", Symbol (Close, "\u2223")
        "\\|", Symbol (Open, "\u2225")
        "\\|", Symbol (Close, "\u2225")
        "\\vert", Symbol (Open, "\u2223")
        "\\vert", Symbol (Close, "\u2223")
        "\\Vert", Symbol (Open, "\u2225")
        "\\Vert", Symbol (Close, "\u2225")
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
        "'", Symbol (Ord, "\u02B9")
        "''", Symbol (Ord, "\u02BA")
        "'''", Symbol (Ord, "\u2034")
        "''''", Symbol (Ord, "\u2057")
        "=", Symbol (Rel, "=")
        ":=", Symbol (Rel, ":=")
        "\\mid", Symbol (Bin, "\u2223")
        "\\parallel", Symbol (Rel, "\u2225")
        "\\backslash", Symbol (Bin, "\u2216")
        "/", Symbol (Bin, "/")
        "\\setminus", Symbol (Bin, "\\")
        "\\times", Symbol (Bin, "\u00D7")
        "\\alpha", Symbol (Ord, "\u03B1")
        "\\beta",  Symbol (Ord, "\u03B2")
        "\\chi",   Symbol (Ord, "\u03C7")
        "\\delta", Symbol (Ord, "\u03B4")
        "\\Delta", Symbol (Op, "\u0394")
        "\\epsilon", Symbol (Ord, "\u03B5")
        "\\varepsilon", Symbol (Ord, "\u025B")
        "\\eta", Symbol (Ord, "\u03B7")
        "\\gamma", Symbol (Ord, "\u03B3")
        "\\Gamma", Symbol (Op, "\u0393") 
        "\\iota",  Symbol (Ord, "\u03B9")
        "\\kappa", Symbol (Ord, "\u03BA")
        "\\omega", Symbol (Ord, "\u03C9")
        "\\Omega", Symbol (Op, "\u03A9")
        "\\lambda", Symbol (Ord, "\u03BB")
        "\\Lambda", Symbol (Op, "\u039B") 
        "\\varphi", Symbol (Ord, "\u03D5")
        "\\mu",  Symbol (Ord, "\u03BC")
        "\\nu",  Symbol (Ord, "\u03BD")
        "\\phi", Symbol (Ord, "\u03C6")
        "\\Phi", Symbol (Op, "\u03A6") 
        "\\pi",  Symbol (Ord, "\u03C0")
        "\\Pi",  Symbol (Op, "\u03A0") 
        "\\psi", Symbol (Ord, "\u03C8")
        "\\Psi", Symbol (Ord, "\u03A8")
        "\\rho", Symbol (Ord, "\u03C1")
        "\\sigma", Symbol (Ord, "\u03C3")
        "\\Sigma", Symbol (Op, "\u03A3") 
        "\\theta", Symbol (Ord, "\u03B8")
        "\\tau", Symbol (Ord, "\u03C4")
        "\\vartheta", Symbol (Ord, "\u03D1")
        "\\Theta", Symbol (Op, "\u0398") 
        "\\upsilon", Symbol (Ord, "\u03C5")
        "\\xi", Symbol (Ord, "\u03BE")
        "\\Xi", Symbol (Op, "\u039E") 
        "\\zeta", Symbol (Ord, "\u03B6")
        "\\frac12", Symbol (Ord, "\u00BD")
        "\\frac14", Symbol (Ord, "\u00BC")
        "\\frac34", Symbol (Ord, "\u00BE")
        "\\frac13", Symbol (Ord, "\u2153")
        "\\frac23", Symbol (Ord, "\u2154")
        "\\frac15", Symbol (Ord, "\u2155")
        "\\frac25", Symbol (Ord, "\u2156")
        "\\frac35", Symbol (Ord, "\u2157")
        "\\frac45", Symbol (Ord, "\u2158")
        "\\frac16", Symbol (Ord, "\u2159")
        "\\frac56", Symbol (Ord, "\u215A")
        "\\frac18", Symbol (Ord, "\u215B")
        "\\frac38", Symbol (Ord, "\u215C")
        "\\frac58", Symbol (Ord, "\u215D")
        "\\frac78", Symbol (Ord, "\u215E")
        "\\pm", Symbol (Bin, "\u00B1")
        "\\mp", Symbol (Bin, "\u2213")
        "\\triangleleft", Symbol (Bin, "\u22B2")
        "\\triangleright", Symbol (Bin, "\u22B3")
        "\\cdot", Symbol (Bin, "\u22C5")
        "\\star", Symbol (Bin, "\u22C6")
        "\\ast", Symbol (Bin, "\u002A")
        "\\times", Symbol (Bin, "\u00D7")
        "\\div", Symbol (Bin, "\u00F7")
        "\\circ", Symbol (Bin, "\u2218")
        "\\bullet", Symbol (Bin, "\u2022")
        "\\oplus", Symbol (Bin, "\u2295")
        "\\ominus", Symbol (Bin, "\u2296")
        "\\otimes", Symbol (Bin, "\u2297")
        "\\bigcirc", Symbol (Bin, "\u25CB")
        "\\oslash", Symbol (Bin, "\u2298")
        "\\odot", Symbol (Bin, "\u2299")
        "\\land", Symbol (Bin, "\u2227")
        "\\wedge", Symbol (Bin, "\u2227")
        "\\lor", Symbol (Bin, "\u2228")
        "\\vee", Symbol (Bin, "\u2228")
        "\\cap", Symbol (Bin, "\u2229")
        "\\cup", Symbol (Bin, "\u222A")
        "\\sqcap", Symbol (Bin, "\u2293")
        "\\sqcup", Symbol (Bin, "\u2294")
        "\\sqrt", Symbol (Bin, "\u221A")
        "\\uplus", Symbol (Bin, "\u228E")
        "\\amalg", Symbol (Bin, "\u2210")
        "\\bigtriangleup", Symbol (Bin, "\u25B3")
        "\\bigtriangledown", Symbol (Bin, "\u25BD")
        "\\dag", Symbol (Bin, "\u2020")
        "\\lhd", Symbol (Bin, "\u22B2")
        "\\rhd", Symbol (Bin, "\u22B3")
        "\\neq", Symbol (Rel, "\u2260")
        "\\leq", Symbol (Rel, "\u2264")
        "\\dagger",  Symbol (Bin, "\u2020")
        "\\ddagger", Symbol (Bin, "\u2021")
        "\\unlhd", Symbol (Bin, "\u22B4")
        "\\unrhd", Symbol (Bin, "\u22B5")
        "\\ddag",  Symbol (Bin, "\u2021")
        "\\lt", Symbol (Rel, "<")
        "\\gt", Symbol (Rel, ">")
        "\\ne", Symbol (Rel, "\u2260")
        "\\le", Symbol (Rel, "\u2264")
        "\\leqslant", Symbol (Rel, "\u2264")
        "\\ge", Symbol (Rel, "\u2265")
        "\\geq", Symbol (Rel, "\u2265")
        "\\geqslant", Symbol (Rel, "\u2265")
        "\\ll", Symbol (Rel, "\u226A")
        "\\gg", Symbol (Rel, "\u226B")
        "\\doteq", Symbol (Rel, "\u2250")
        "\\equiv", Symbol (Rel, "\u2261")
        "\\prec",  Symbol (Rel, "\u227A")
        "\\succ",  Symbol (Rel, "\u227B")
        "\\preceq", Symbol (Rel, "\u227C")
        "\\succeq", Symbol (Rel, "\u227D")
        "\\subset", Symbol (Rel, "\u2282")
        "\\supset", Symbol (Rel, "\u2283")
        "\\subseteq", Symbol (Rel, "\u2286")
        "\\supseteq", Symbol (Rel, "\u2287")
        "\\sqsubset", Symbol (Rel, "\u228F")
        "\\sqsupset", Symbol (Rel, "\u2290")
        "\\sqsubseteq", Symbol (Rel, "\u2291")
        "\\sqsupseteq", Symbol (Rel, "\u2292")
        "\\sim",   Symbol (Rel, "\u223C")
        "\\simeq", Symbol (Rel, "\u2243")
        "\\approx", Symbol (Rel, "\u2248")
        "\\bowtie", Symbol (Rel, "\u22C8")
        "\\models", Symbol (Rel, "\u22A8")
        "\\propto", Symbol (Rel, "\u221D")
        "\\in", Symbol (Rel, "\u2208")
        "\\ni", Symbol (Rel, "\u220B")
        "\\owns",  Symbol (Rel, "\u220B")
        "\\vdash", Symbol (Rel, "\u22A2")
        "\\cong",  Symbol (Rel, "\u2245")
        "\\Join",  Symbol (Rel, "\u22C8")
        "\\dashv", Symbol (Rel, "\u22A3")
        "\\perp",  Symbol (Rel, "\u22A5")
        "\\smile", Symbol (Rel, "\u2323")
        "\\frown", Symbol (Rel, "\u2322")
        "\\asymp", Symbol (Rel, "\u224D")
        "\\notin", Symbol (Rel, "\u2209")
        "\\gets",  Symbol (Rel, "\u2190")
        "\\leftarrow", Symbol (Rel, "\u2190")
        "\\to", Symbol (Rel, "\u2192")
        "\\rightarrow", Symbol (Rel, "\u2192")
        "\\leftrightarrow", Symbol (Rel, "\u2194")
        "\\uparrow", Symbol (Rel, "\u2191")
        "\\downarrow", Symbol (Rel, "\u2193")
        "\\updownarrow", Symbol (Rel, "\u2195")
        "\\Leftarrow", Symbol (Rel, "\u21D0")
        "\\Rightarrow", Symbol (Rel, "\u21D2")
        "\\Leftrightarrow", Symbol (Rel, "\u21D4")
        "\\iff", Symbol (Rel, "\u21D4")
        "\\Uparrow", Symbol (Rel, "\u21D1")
        "\\Downarrow", Symbol (Rel, "\u21D3")
        "\\Updownarrow", Symbol (Rel, "\u21D5")
        "\\mapsto", Symbol (Rel, "\u21A6")
        "\\longleftarrow", Symbol (Rel, "\u2190")
        "\\longrightarrow", Symbol (Rel, "\u2192")
        "\\longleftrightarrow", Symbol (Rel, "\u2194")
        "\\Longleftarrow", Symbol (Rel, "\u21D0")
        "\\Longrightarrow", Symbol (Rel, "\u21D2")
        "\\Longleftrightarrow", Symbol (Rel, "\u21D4")
        "\\longmapsto", Symbol (Rel, "\u21A6")
        "\\sum", Symbol (Op, "\u2211")
        "\\prod", Symbol (Op, "\u220F")
        "\\bigcap", Symbol (Op, "\u22C2")
        "\\bigcup", Symbol (Op, "\u22C3")
        "\\bigvee", Symbol (Op, "\u22C1")
        "\\coprod", Symbol (Op, "\u2210")
        "\\bigwedge", Symbol (Op, "\u22C0")
        "\\bigsqcap", Symbol (Op, "\u2A05")
        "\\bigsqcup", Symbol (Op, "\u2A06")
        "\\bigoplus", Symbol (Op, "\u2A01")
        "\\biguplus", Symbol (Op, "\u2A04")
        "\\bigodot",  Symbol (Op, "\u2A00")
        "\\int", Symbol (Op, "\u222B")
        "\\bigotimes", Symbol (Op, "\u2A02")
        "\\iint", Symbol (Op, "\u222C")
        "\\oint", Symbol (Op, "\u222E")
        "\\dots", Symbol (Ord, "\u2026")
        "\\iiint", Symbol (Op, "\u222D")
        "\\prime", Symbol (Ord, "\u2032")
        "\\ldots", Symbol (Ord, "\u2026")
        "\\cdots", Symbol (Ord, "\u22EF")
        "\\vdots", Symbol (Ord, "\u22EE")
        "\\ddots", Symbol (Ord, "\u22F1")
        "\\forall", Symbol (Op, "\u2200")
        "\\exists", Symbol (Op, "\u2203")
        "\\Re", Symbol (Ord, "\u211C")
        "\\Im", Symbol (Ord, "\u2111")
        "\\wp", Symbol (Ord, "\u2118")
        "\\aleph", Symbol (Ord, "\u2135")
        "\\infty", Symbol (Ord, "\u221E")
        "\\hbar", Symbol (Ord, "\u210F")
        "\\ell", Symbol (Ord, "\u2113")
        "\\emptyset", Symbol (Ord, "\u2205")
        "\\partial", Symbol (Ord, "\u2202")
        "\\nabla", Symbol (Ord, "\u2207")
        "\\angle", Symbol (Ord, "\u2220")
        "\\diamond", Symbol (Op, "\u22C4")
        "\\triangle", Symbol (Ord, "\u25B3")
        "\\therefore", Symbol (Pun, "\u2234")
        "\\Diamond", Symbol (Op, "\u25C7")
        "\\neg", Symbol (Op, "\u00AC")
        "\\lnot", Symbol (Ord, "\u00AC")
        "\\bot", Symbol (Ord, "\u22A5")
        "\\top", Symbol (Ord, "\u22A4")
        "\\square", Symbol (Ord, "\u25AB")
        "\\Box", Symbol (Op, "\u25A1")
        "\\wr", Symbol (Ord, "\u2240")
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
