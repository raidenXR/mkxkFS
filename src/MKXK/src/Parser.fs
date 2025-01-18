namespace MKXK
open System
open System.Linq
open System.Text
open Lexer
open ExprTree

type Symbols = {
    constants: seq<string>
    variables: seq<string>
    functions: seq<string>
}

type Parser(source: string, symbols:Symbols) = 
    let mutable pos = 0
    let mutable tokens: list<Token> = []
    let append (token: Token) = tokens <- tokens @ [token]
    // let tokens = new System.Collections.Generic.List<Token>(20)
    let symbols' = Seq.concat [symbols.variables; symbols.constants; symbols.functions] |> Array.ofSeq

    do
        let lexer = Lexer(source, symbols')
        let mutable token = lexer.nextToken()
        while token.IsSome do
            let _token = token.Value
            if _token.Id <> Whitespace && _token.Id <> Bad && _token.Id <> Comment then
                // if tokens.Count > 0 then
                if not tokens.IsEmpty then
                    let prev_token = List.last tokens
                    let prev = prev_token.Id = Identifier || prev_token.Id = TokenId.Number || prev_token.Id = RightParen                         
                    let cur = _token.Id = Identifier || _token.Id = LeftParen || _token.Id = LeftBrace
                    if prev && cur then
                        // tokens.Add(Token(_token.Pos, "\\cdot", Cdot)) 
                        append (Token(_token.Pos, "\\cdot", Cdot))
                // tokens.Add(_token)
                append (_token)
            token <- lexer.nextToken()


    let peek (offset: int) = 
        let index = pos + offset
        if index >= tokens.Length then tokens.Last() else tokens[index]
    
    let current() = peek 0

    let nextToken() = 
        let _current = current()
        pos <- pos + 1
        _current

    let matchToken(id: TokenId) =
        let _current = current()
        if _current.Id = id 
        then nextToken() 
        else
            Console.WriteLine $"not matched token. current token is: {_current.Id}, expected token is: {id}" 
            Token(_current.Pos, _current.Str, id)

    
    let rec parseExpr() = 
        if (peek 0).Id = Identifier && (peek 1).Id = TokenId.Equal 
        then parseAssignment()
        else parseBinary 0

    and parseAssignment() = 
        let identifier = nextToken()
        let lhs = identifier.Str
        let operator = nextToken() |> ignore  // skip equal
        let rhs = parseExpr()
        Assignment (lhs, rhs)

    and parseBinary(parent_precedence: int) = 
        let op_precedence = 0
        // let mutable left: Expr option = None 
        let op_precedence = unaryOpPrecedence (current().Id)
        let mutable left = ExprNone

        if op_precedence <> 0 && op_precedence >= parent_precedence then
            let operator = nextToken().Id
            let operand = parseExpr()
            left <- Unary (operand, ref operator)
        else 
            left <- parsePrimary()
        
        let rec loop() =
            let precedence = binaryOpPrecedence (current().Id)
            let _condition = precedence = 0 || precedence <= parent_precedence
            if (not _condition) then
                let operator = nextToken().Id
                let right = parseBinary precedence
                left <- Binary (left, right, ref operator)
                loop()

        loop()
        left


    and parsePrimary() = 
        match current().Id with
        | LeftParen -> parseParenthesized()
        | LeftBrace -> parseBraces()
        | Pipe -> parseEnclosure()
        | TokenId.Frac -> parseFrac()
        | Identifier -> 
            let literal = nextToken()
            if symbols.variables.Contains literal.Str then Symbol (ref literal.Str)
            elif symbols.constants.Contains literal.Str then Constant (ref literal.Str)
            elif symbols.variables.Contains literal.Str then Symbol (ref literal.Str)
            else Symbol (ref literal.Str)
        | TokenId.Number -> 
            let literal = nextToken()
            let mutable _value = 0.0
            match (Double.TryParse(literal.Str, &_value)) with
            | true -> Number (ref _value)
            | false -> failwith $"-{literal.Str}- failed to parse"
        | Log| Ln| Exp| Sqrt -> 
            let operator = nextToken().Id
            let operand = parseExpr()
            Unary (operand, ref operator)
        | TokenId.Sum -> parseSum()
        | TokenId.Prod -> parseProd()
        | TokenId.Int -> parseInt()
        | TokenId.Diff -> parseDiff()
        | TokenId.ODE -> parseODE()
        | TokenId.PDE -> parsePDE()
        | _ -> failwith $"{current().Id} not implemented"

    and parseParenthesized() = 
        // let left = nextToken()
        ignore (matchToken LeftParen)
        let expr = parseExpr()
        // let right = nextToken()
        ignore (matchToken RightParen)
        Parenthesized (expr)

    and parseBraces() = 
        let left = matchToken LeftBrace
        let enclosed_statement = (peek 0).Id = Identifier && (peek 1).Id = Equal && (peek 2).Id = TokenId.Number
        let expr = if enclosed_statement then parseStatement() else parseExpr()
        let right = matchToken RightBrace
        Braces (expr)

    and parseFrac() =
        ignore (nextToken())  // skip Frac and continue to braces
        ignore (matchToken LeftBrace)
        let left = parseExpr()
        ignore (matchToken RightBrace)

        ignore (matchToken LeftBrace)
        let right = parseExpr()
        ignore (matchToken RightBrace)

        Frac (left, right)

    and parseEnclosure() =
        let left = matchToken Pipe
        let expr = parseExpr()
        let right = matchToken Pipe
        Enclosure (expr)

    and parseStatement() =
        let literal = nextToken()
        ignore (matchToken Equal) // skip Equal sign
        let _value = nextToken()
        let mutable _n = 0.0
        if Double.TryParse(_value.Str, &_n) 
        then Number (ref _n)
        else failwith $"could not parse number correcty: {_value.Str}"

    and parseSum() = 
        ignore (nextToken())  // skip prod and continue to braces
        ignore (matchToken LeftBrace)
        let i = parseExpr()
        ignore (matchToken RightBrace)

        ignore (matchToken LeftBrace)
        let n = parseExpr()
        ignore (matchToken RightBrace)

        ignore (matchToken LeftBrace)
        let f = parseExpr()
        ignore (matchToken RightBrace)
        
        Sum (i, n, f)

    and parseProd() = 
        ignore (nextToken())  // skip prod and continue to braces
        ignore (matchToken LeftBrace)
        let i = parseExpr()
        ignore (matchToken RightBrace)

        ignore (matchToken LeftBrace)
        let n = parseExpr()
        ignore (matchToken RightBrace)

        ignore (matchToken LeftBrace)
        let f = parseExpr()
        ignore (matchToken RightBrace)
        
        Prod (i, n, f)

    and parseInt() = 
        ignore (nextToken())  // skip int and continue to braces

        match current().Id with
        | Accent | Underscore ->
            let lhs = nextToken()
            ignore (matchToken LeftBrace)
            let lhs_expr = parseExpr()
            ignore (matchToken RightBrace)

            let rhs = nextToken()
            ignore (matchToken LeftBrace)
            let rhs_expr = parseExpr()
            ignore (matchToken RightBrace)
        

            let a = if lhs.Id = TokenId.Accent then rhs_expr else lhs_expr
            let b = if rhs.Id = TokenId.Underscore then lhs_expr else rhs_expr
            // printfn "%s" (string a)
            // printfn "%s" (string b)
            let e = parseExpr()
            let dx = nextToken()  // ignore the d part of the target string
            Int (a, b, e, dx.Str[1..])
        | _ -> 
            let e = parseExpr()
            let dx = nextToken()  // ignore the d part of the target string
            IntIndefinite (e, dx.Str[1..])        
        

    and parseDiff() = 
        ignore (nextToken())  // skip ode/pde and continue to braces
        ignore (matchToken LeftBrace)
        let upper = parseExpr()
        ignore (matchToken RightBrace)

        ignore (matchToken LeftBrace)
        // get target, it must be symbol        
        // let t = match parseExpr() with | Symbol s -> s | _ -> failwith "must be symbol"
        let t = nextToken()
        ignore (matchToken RightBrace)

        Diff (upper, t.Str)

    and parseODE() = 
        ignore (nextToken())  // skip ode/pde and continue to braces
        ignore (matchToken LeftBrace)
        let upper = parseExpr()
        ignore (matchToken RightBrace)

        ignore (matchToken LeftBrace)
        // get target, it must be symbol        
        // let t = match parseExpr() with | Symbol s -> s | _ -> failwith "must be symbol"
        let t = nextToken()
        ignore (matchToken RightBrace)

        ODE (upper, t.Str)
        
    and parsePDE() = 
        ignore (nextToken())  // skip ode/pde and continue to braces
        ignore (matchToken LeftBrace)
        let upper = parseExpr()
        ignore (matchToken RightBrace)

        ignore (matchToken LeftBrace)
        let lower = parseExpr()
        ignore (matchToken RightBrace)

        PDE (upper, lower)                

    member x.Tokens with get() = tokens

    member x.Symbols with get() = symbols'

    /// parses some latex str and returns the root(Expr) of the AST
    member this.ParseExpr() = if tokens.IsEmpty then ExprNone else parseExpr()


module Parser =
    /// Parses a tex string and returns an Expr
    let parse (symbols:Symbols) (tex:string) :Expr = 
        let parser = Parser(tex, symbols)
        parser.ParseExpr()

    /// parses a file that contains gnu-points columns
    let parseXY (data: array<string>) =
        let x = System.Collections.Generic.List<float>()
        let y = System.Collections.Generic.List<float>()
        for line in data do
            if line.Contains "#" || line.Length < 3 then ignore ()
            else 
                let values = line.Split() |> Array.filter (fun x -> x.Length > 0)
                x.Append (float values[0]) |> ignore
                y.Append (float values[1]) |> ignore
        x.ToArray(), y.ToArray()
                
        
    /// parses a file that contains gnu-points columns
    let parseXYZ (data: array<string>) =
        let x = System.Collections.Generic.List<float>()
        let y = System.Collections.Generic.List<float>()
        let z = System.Collections.Generic.List<float>()
        for line in data do
            if line.Contains "#" || line.Length < 5 then ignore ()
            else 
                let values = line.Split() |> Array.filter (fun x -> x.Length > 0)
                x.Append (float values[0]) |> ignore
                y.Append (float values[1]) |> ignore
                z.Append (float values[2]) |> ignore
        x.ToArray(), y.ToArray(), z.ToArray()
         
        
