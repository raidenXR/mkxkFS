namespace NotationFS
open System
open System.Collections.Generic
open System.Linq
open Notation

type Lexer(src: string) =
    let mutable pos = 0
    
    let peek offset = 
        let idx = pos + offset
        if idx >= src.Length then '\x00' else src[idx]

    let current () = peek 0

    let lookAhead () = peek 1

    let advance () = pos <- pos + 1

    let offset dx = pos <- pos + dx

    let nextKeyword () = 
        let start = pos
        advance ()  // skip \
        while (System.Char.IsLetter(current())) do advance()    
        src[start..pos - 1]

    // bad copy of imperative C# code
    let nextWord () =
        let start = pos
        let len = src.Length - pos
        let slice = src.AsSpan(pos, len)

        let mutable i = 0
        let mutable break' = false
        while i < slice.Length && not break' do
            match slice[i] with
            | ' ' | '_' | '^' | '(' | ')' | '{' | '}' -> 
                pos <- pos + i
                break' <- true
            | _ -> ()
            i <- i + 1
        if break' then
            slice.Slice(0, i - 1).ToString() 
        else
            pos <- pos + len
            slice.Slice(0, len - 1).ToString()


    let matchSequence (keys:array<string>) =
        let mutable key = ""
        let mutable res = false

        let mutable i = 0
        let mutable break' = false
        while i < keys.Length && not break' do
            let str = keys[i]
            if src.AsSpan(pos, str.Length).Equals(str, StringComparison.Ordinal) then
                break' <- true
                key <- str
                pos <- pos + str.Length
                res <- true 
            i <- i + 1
            
        res,key


    /// implement this
    member _.lex () = 
        let start = pos

        if pos >= src.Length then Token(pos, "eof", TokenId.Eof)

        elif Char.IsWhiteSpace(current()) then
            while Char.IsWhiteSpace(current()) do advance()
            Token(pos, src[start..pos - 1], TokenId.WhiteSpace)

        elif Char.IsDigit(current()) then
            while Char.IsDigit(current()) || (current() = '.' && Char.IsDigit(lookAhead())) do advance()
            Token(pos, src[start..pos - 1], TokenId.Number)

        elif Char.IsLetter(current()) then
            while Char.IsLetter(current()) do advance()
            Token(pos, src[start..pos - 1], TokenId.Identifier)

        else 
            match current() with 
            | '=' | '+' | '-' | '*' | '/' -> advance(); Token(pos, src[start..pos - 1], TokenId.MathOperator)
            | '{' -> advance(); Token(pos, src[start..pos - 1], TokenId.GroupedOpen)
            | '}' -> advance(); Token(pos, src[start..pos - 1], TokenId.GroupedClose)
            | '^' -> advance(); Token(pos, src[start..pos - 1], TokenId.Up)
            | '_' -> advance(); Token(pos, src[start..pos - 1], TokenId.Down)
            | '|' | '(' | '[' -> advance(); Token(pos, src[start..pos - 1], TokenId.Open)
            | ')' | ']' -> advance(); Token(pos, src[start..pos - 1], TokenId.Close)
            | '\\' ->
                match (peek 1) with 
                | '|' -> advance(); advance(); Token(pos, "\\|", TokenId.Open)
                | '{' -> advance(); advance(); Token(pos, "\\{", TokenId.Open)
                | '}' -> advance(); advance(); Token(pos, "\\}", TokenId.Close)
                | _ -> 
                    let next_word = nextWord ()
                    // Console.WriteLine next_word

                    if diacriticals.ContainsKey next_word then
                        let e = diacriticals[next_word] (Expr.Text "")
                        match e with
                        | Over _ -> Token(pos, next_word, TokenId.Over)
                        | Under _ -> Token(pos, next_word, TokenId.Under)
                        | _ -> failwith ("invalid case in diacritical " + next_word)
                        
                    elif scalers.ContainsKey next_word then
                        Token(pos, next_word, TokenId.Scaled)
                        
                    elif enclosures.ContainsKey next_word then
                        match enclosures[next_word] with
                        | Symbol (t,_) -> 
                            match t with
                            | Open -> Token(pos, next_word, TokenId.Open)
                            | Close -> Token(pos, next_word, TokenId.Close)
                            | _ -> failwith ("invalid case in enclosure" + next_word)
                        | _ -> Token(pos, "", TokenId.Bad)
                        
                    elif List.contains next_word binaryops then
                        Token(pos, next_word, TokenId.Binary)

                    elif symbols.ContainsKey next_word then
                        match symbols[next_word] with
                        | Symbol (t,s) -> Token(pos, next_word, TokenId.Symbol)
                        | Space s -> Token(pos, next_word, TokenId.Space)
                        | MathOperator s -> Token(pos, next_word, TokenId.MathOperator)
                        | _ -> Token(pos, next_word, Bad) 
                        
                    else 
                        advance ()
                        Token(pos, "", TokenId.Bad)
            | _ ->
                advance () 
                Token(pos, "", TokenId.Bad)


        member x.tokens() =
            seq {
                let mutable c = x.lex()
                while c.Id <> TokenId.Eof do yield c; c <- x.lex() 
            }



type Parser(src:string) =
    let tokens = List<Token>(20)
    let mutable pos = 0
    let mutable node_n = 0

    do
        let lexer = Lexer(src)
        for token in lexer.tokens() do
            if token.Id = TokenId.Bad || token.Id = TokenId.WhiteSpace then ignore ()
            else tokens.Add token


    let peek offset =
        let index = pos + offset
        if index >= tokens.Count then Token(index, "Eof", TokenId.Eof) else tokens[index]

    let current () = peek 0

    let lookAhead () = peek 1

    let nextToken () = 
        let token = peek 0
        pos <- pos + 1
        node_n <- node_n + 1
        token

    let matchToken (kind:TokenId) =
        if (current()).Id = kind then nextToken () else Token(pos, (current()).Str, kind)


    let rec parseExpr () :Expr =
        let token = nextToken()        
        match token.Id with
        | TokenId.Number -> Number (token.Str)
            // let e = Number(token.Str)
            // e
        | TokenId.Identifier -> Identifier (token.Str)
            // let e = Identifier(nextToken().Str)
            // exprs.Add e
            // e
        | TokenId.Over 
        | TokenId.Under 
        | TokenId.Space   
        | TokenId.Scaled -> failwith "Over, Under not implemented yet"
        | TokenId.MathOperator
        | TokenId.Symbol -> symbols[token.Str] 
            // let e = symbols[nextToken().Str]
            // exprs.Add e
            // e
        | TokenId.Binary -> 
            // let c = nextToken()
            let l = parseExpr()
            let r = parseExpr()
            Binary (token.Str, l, r)
            // let e = Binary(token.Str,l,r)
            // exprs.Add e
            // e
        | TokenId.GroupedOpen ->                
            let exprs = List<Expr>()
            while current().Id <> TokenId.GroupedClose && current().Id <> TokenId.Eof do
                exprs.Add (parseExpr())
            ignore (nextToken())
            Grouped (exprs)
            // ignore (nextToken())    // ignore {
            // let g = List<Expr>()
            // while current().Id <> TokenId.GroupedClose do 
                // g.Add (parseExpr())
            // ignore (nextToken()) // ignore }
            // let e = Grouped(g)
            // exprs.Add e
            // e
        | TokenId.GroupedClose -> RightBrace
            // ignore (nextToken())   // ignore }
            // RightBrace
            // failwith $"GroupedClose - token in pos: {current().Pos}, last token: {exprs.Last().ToString()}"
        | TokenId.Open 
        | TokenId.Close -> enclosures[token.Str] 
            // let e = enclosures[token.Str]
            // exprs.Add e
            // e
        | TokenId.Up ->
            let n = parseExpr()
            Up (ExprNone, n)
            // ignore (nextToken())    // ignore ^
            // let b = exprs.Last()
            // let b = ExprNone
            // let n = parseExpr()
            // let e = Up(b, n)
            // exprs.Add e
            // e                
        | TokenId.Down -> 
            let n = parseExpr()
            Down (ExprNone, n)
            // ignore (nextToken())   // ignore _
            // let b = exprs.Last()
            // let b = ExprNone
            // let n = parseExpr()
            // let e = Down(b, n)
            // exprs.Add e
            // e
        | TokenId.Eof -> Eof
        | _ -> failwith $"{(current()).Id} is not implemented"

    member x.exprs() :List<Expr> =
        // WHEN THE LAST NODE IS REACHED, IT WILL RETURN IT REPEATEDLY, TO AVOID -OVERFLOW-
        // WHEN ParseExpr() recursively it moves forward, while the foreach token in tokens enumerators
        // indexes to previous token.
        // ##################################################################                
        // WARNING: if called multiple time, it will append to same sxpressions List<Expr>
        // ##################################################################                
        let exprs = List<Expr>(20)
        while pos < tokens.Count do exprs.Add(parseExpr())
        exprs


    // member _.createAST () = 
    //     for token in tokens do expressions.Add (parseExpr(expressions))
    //     let root = parseExpr (expressions)
    //     root
