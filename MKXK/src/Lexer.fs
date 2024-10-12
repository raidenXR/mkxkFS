namespace MKXK
    module Lexer =
        open System
        open System.Text

        let private text_ops = [|
            "\\textrm" 
            "\\mathrm" 
            "\\text"   
            "\\mbox"   
            "\\mathbf" 
            "\\textbf" 
            "\\mathit" 
            "\\textit" 
            "\\mathtt" 
            "\\texttt" 
            "\\mathsf" 
            "\\mathbb" 
            "\\mathcal" 
            "\\mathfrak" 
        |]

        let private binary_ops = [|
            "\\frac" 
            "\\tfrac" 
            "\\dfrac" 
            "\\stackrel" 
            "\\overset" 
            "\\underset" 
            "\\binom"
        |]

        let private diacriticals = [|
            "\\acute"
            "\\grave"
            "\\breve"
            "\\check"
            "\\dot"
            "\\ddot"
            "\\mathring"
            "\\vec"
            "\\overrightarrow"
            "\\overleftarrow"
            "\\hat"
            "\\widehat"
            "\\tilde"
            "\\widetilde"
            "\\bar"
            "\\overbrace"
            "\\overbracket"
            "\\overline"
            "\\underbrace"
            "\\underbracket"
            "\\underline"   
        |]

        let private scalers = [|
            "\\bigg" 
            "\\Bigg" 
            "\\big" 
            "\\Big" 
            "\\biggr" 
            "\\Biggr" 
            "\\bigr" 
            "\\Bigr" 
            "\\biggl" 
            "\\Biggl" 
            "\\bigl" 
            "\\Bigl"             
        |]

        let private enclosures = [|
            "(" 
            ")" 
            "[" 
            "]" 
            "\\{" 
            "\\}" 
            "\\lbrack" 
            "\\lbrace" 
            "\\rbrack" 
            "\\rbrace" 
            "\\llbracket" 
            "\\rrbracket" 
            "\\langle" 
            "\\rangle" 
            "\\lfloor" 
            "\\rfloor" 
            "\\lceil" 
            "\\rceil" 
            "|" 
            "|" 
            "\\|" 
            "\\|" 
            "\\vert" 
            "\\vert" 
            "\\Vert" 
            "\\Vert" 
        |]

        let private symbols = [|
            "\\mid" 
            "\\parallel" 
            "\\backslash" 
            "\\setminus"
            "\\times" 
            "\\alpha" 
            "\\beta" 
            "\\chi" 
            "\\delta" 
            "\\Delta" 
            "\\epsilon" 
            "\\varepsilon" 
            "\\eta" 
            "\\gamma" 
            "\\Gamma" 
            "\\iota" 
            "\\kappa" 
            "\\lambda" 
            "\\Lambda" 
            "\\mu" 
            "\\nu" 
            "\\omega" 
            "\\Omega" 
            "\\phi" 
            "\\varphi" 
            "\\Phi" 
            "\\pi" 
            "\\Pi" 
            "\\psi" 
            "\\Psi" 
            "\\rho" 
            "\\sigma" 
            "\\Sigma" 
            "\\tau" 
            "\\theta" 
            "\\vartheta" 
            "\\Theta" 
            "\\upsilon" 
            "\\xi" 
            "\\Xi" 
            "\\zeta" 
            "\\frac12" 
            "\\frac14" 
            "\\frac34" 
            "\\frac13" 
            "\\frac23" 
            "\\frac15" 
            "\\frac25" 
            "\\frac35" 
            "\\frac45" 
            "\\frac16" 
            "\\frac56" 
            "\\frac18" 
            "\\frac38" 
            "\\frac58" 
            "\\frac78" 
            "\\pm" 
            "\\mp" 
            "\\triangleleft" 
            "\\triangleright" 
            "\\cdot" 
            "\\star" 
            "\\ast" 
            "\\times" 
            "\\div" 
            "\\circ" 
            "\\bullet" 
            "\\oplus" 
            "\\ominus" 
            "\\otimes" 
            "\\bigcirc" 
            "\\oslash" 
            "\\odot" 
            "\\land"  
            "\\wedge"  
            "\\lor"  
            "\\vee"  
            "\\cap"  
            "\\cup"  
            "\\sqcap"  
            "\\sqcup"  
            "\\uplus"  
            "\\amalg"  
            "\\bigtriangleup"  
            "\\bigtriangledown"  
            "\\dag"  
            "\\dagger"  
            "\\ddag"  
            "\\ddagger"  
            "\\lhd"  
            "\\rhd"  
            "\\unlhd"  
            "\\unrhd"  
            "\\lt" 
            "\\gt" 
            "\\ne"  
            "\\neq"  
            "\\le"  
            "\\leq"  
            "\\leqslant"  
            "\\ge"  
            "\\geq"  
            "\\geqslant"  
            "\\equiv"  
            "\\ll"  
            "\\gg"  
            "\\doteq"  
            "\\prec"  
            "\\succ" 
            "\\preceq" 
            "\\succeq" 
            "\\subset" 
            "\\supset" 
            "\\subseteq" 
            "\\supseteq" 
            "\\sqsubset" 
            "\\sqsupset" 
            "\\sqsubseteq" 
            "\\sqsupseteq" 
            "\\sim" 
            "\\simeq" 
            "\\approx" 
            "\\cong" 
            "\\Join" 
            "\\bowtie" 
            "\\in" 
            "\\ni" 
            "\\owns" 
            "\\propto" 
            "\\vdash" 
            "\\dashv" 
            "\\models" 
            "\\perp" 
            "\\smile" 
            "\\frown" 
            "\\asymp" 
            "\\notin" 
            "\\gets" 
            "\\leftarrow" 
            "\\to" 
            "\\rightarrow" 
            "\\leftrightarrow" 
            "\\uparrow" 
            "\\downarrow" 
            "\\updownarrow" 
            "\\Leftarrow" 
            "\\Rightarrow" 
            "\\Leftrightarrow" 
            "\\iff" 
            "\\Uparrow" 
            "\\Downarrow" 
            "\\Updownarrow" 
            "\\mapsto" 
            "\\longleftarrow" 
            "\\longrightarrow" 
            "\\longleftrightarrow" 
            "\\Longleftarrow" 
            "\\Longrightarrow" 
            "\\Longleftrightarrow" 
            "\\longmapsto" 
            "\\sum" 
            "\\prod" 
            "\\bigcap" 
            "\\bigcup" 
            "\\bigwedge" 
            "\\bigvee" 
            "\\bigsqcap" 
            "\\bigsqcup" 
            "\\coprod" 
            "\\bigoplus" 
            "\\bigotimes" 
            "\\bigodot" 
            "\\biguplus" 
            "\\int" 
            "\\iint" 
            "\\iiint" 
            "\\oint" 
            "\\prime" 
            "\\dots" 
            "\\ldots" 
            "\\cdots" 
            "\\vdots" 
            "\\ddots" 
            "\\forall" 
            "\\exists" 
            "\\Re" 
            "\\Im" 
            "\\aleph" 
            "\\hbar" 
            "\\ell" 
            "\\wp" 
            "\\emptyset" 
            "\\infty" 
            "\\partial" 
            "\\nabla" 
            "\\triangle" 
            "\\therefore" 
            "\\angle" 
            "\\diamond" 
            "\\Diamond" 
            "\\neg" 
            "\\lnot" 
            "\\bot" 
            "\\top" 
            "\\square" 
            "\\Box" 
            "\\wr" 
            "\\quad"
            "\\qquad"
        |]

        let private math_operators = [|
            "\\arccos" 
            "\\arcsin" 
            "\\arctan" 
            "\\arg" 
            "\\cos" 
            "\\cosh" 
            "\\cot" 
            "\\coth" 
            "\\csc" 
            "\\deg" 
            "\\det" 
            "\\dim" 
            "\\exp" 
            "\\gcd" 
            "\\hom" 
            "\\inf" 
            "\\ker" 
            "\\lg" 
            "\\lim" 
            "\\liminf" 
            "\\limsup" 
            "\\ln" 
            "\\log" 
            "\\max" 
            "\\min" 
            "\\Pr" 
            "\\sec" 
            "\\sin" 
            "\\sinh" 
            "\\sup" 
            "\\tan" 
            "\\tanh" 
        |]

        let inline isLetter (c: char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') 

        let inline isDigit (c: char) = (c >= '0' && c <= '9')

        let inline isWhiteSpace (c: char) = (c = ' ') || (c = '\n') || (c = '\r') || (c = '\t')


        type Lexer(source: string, symbols': seq<string>) =
            let mutable pos = 0
            // symbols MUST be sorted, to ensure conflicts of symbols that are partially the same
            let symbols = symbols' |> Array.ofSeq |> Array.sortByDescending (fun x -> x.Length)

            let peek (offset: int) =
                let index = pos + offset
                if index >= source.Length then '\x00' else source[index]

            let current() = peek 0

            let lookAhead() = peek 1

            let advance() = pos <- pos + 1

            let offset (dx: int) = pos <- pos + dx

            let matchPattern (pattern: string) =
                if pattern.Length > source.Length - pos then false
                else
                    let slice = source.AsSpan(pos, pattern.Length)
                    if slice.Equals(pattern, StringComparison.Ordinal) then
                        pos <- pos + pattern.Length
                        true
                    else false

            let nextWord() =
                let p = pos
                let mutable n = p + 1
                while isLetter (source[n]) do n <- n + 1
                source[p..(n - 1)]

            // let rec digit_condition() = 
            //     if isDigit(current()) then advance(); digit_condition()
            //     elif isDigit(lookAhead()) then advance(); digit_condition()

            let symbolExists() =                 
                let rec exists idx =
                    if idx >= symbols.Length then false
                    elif matchPattern symbols[idx] then true
                    else exists (idx + 1)
                exists 0    
                    

            member x.lex() =
                let start = pos
                let src = source
                let len = src.Length                                

                if pos >= len then 
                    Token(pos, "eof", Eof)
                
                // slices in F# are INCLUSIVE -> (pos - 1) instead of pos!!!
                elif isWhiteSpace (current()) then
                    while isWhiteSpace (current()) do advance()
                    Token(pos, src[start..(pos - 1)], Whitespace)

                // if pos is still at lhs, parse the lhs fn name as a whole and advance
                // directly to '=' char
                elif current() <> '=' && src[start..].Contains "=" then                    
                    let eql_pos = src[start..].IndexOf "="
                    let lhs = src[start..(eql_pos - 2)].Trim(' ')
                    // Console.WriteLine $"-{lhs}-"
                    pos <- eql_pos - 1
                    Token(start, lhs, Identifier)

                elif symbolExists() then
                // elif (Array.contains )
                    // Console.WriteLine $"symbol_str: -{src[start..(pos - 1)]}-"
                    Token(start, src[start..(pos - 1)], Identifier)                    

                elif isDigit (current()) then   
                    while(isDigit(current()) || (current() = '.' && isDigit(lookAhead()))) do advance()
                    // Console.WriteLine $"pos: {source[pos - 1]}"
                    Token(pos, src[start..(pos - 1)], TokenId.Number)

                elif isLetter (current()) then
                    while isLetter (current()) do advance() 
                    Token(pos, src[start..(pos - 1)], Identifier)

                else match (current()) with
                        | '+' -> advance(); Token(pos, "+", Plus)
                        | '-' -> advance(); Token(pos, "-", Minus)
                        | '*' -> advance(); Token(pos, "*", Star)
                        | '/' -> advance(); Token(pos, "/", Slash)
                        | '=' -> advance(); Token(pos, "=", Equal)
                        | '^' -> advance(); Token(pos, "^", Accent)
                        | '_' -> advance(); Token(pos, "_", Underscore)
                        | '(' -> advance(); Token(pos, "(", LeftParen)
                        | ')' -> advance(); Token(pos, ")", RightParen)
                        | '{' -> advance(); Token(pos, "{", LeftBrace)
                        | '}' -> advance(); Token(pos, "}", RightBrace)
                        | '|' -> advance(); Token(pos, "|", Pipe)
                        | '\\' -> 
                            let next_word = nextWord()
                            // Console.WriteLine $"-{next_word}-"
                            let dx = next_word.Length
                            match next_word with
                            | "\\frac" -> offset dx; Token(start, next_word, TokenId.Frac)
                            | "\\cdot" -> offset dx; Token(start, next_word, Cdot)
                            | "\\log" -> offset dx; Token(start, next_word, Log)
                            | "\\ln" -> offset dx; Token(start, next_word, Ln)
                            | "\\exp" -> offset dx; Token(start, next_word, Exp)
                            | "\\sqrt" -> offset dx; Token(start, next_word, Sqrt)
                            | "\\sum" -> offset dx; Token(start, next_word, TokenId.Sum)
                            | "\\prod" -> offset dx; Token(start, next_word, TokenId.Prod)
                            | "\\int" -> offset dx; Token(start, next_word, TokenId.Int)
                            | "\\ode" -> offset dx; Token(start, next_word, TokenId.ODE)
                            | "\\pde" -> offset dx; Token(start, next_word, TokenId.PDE)
                            | _ -> failwith $"-{next_word}- not implemented"
                        | _ -> Token(pos, "", Bad)


            member x.nextToken() = 
                let _next = x.lex()
                if (_next.Id = Eof || _next.Id = Bad) then None else Some _next
                
