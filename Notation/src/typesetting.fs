namespace NotationFS
open System
open System.Numerics
open SkiaSharp

module Typesetting =

    let private fonts = [
        "Main-Regular"
        "AMS-Regular"
        "Caligraphic-Regular"
        "Fraktur-Regular"
        "SansSerif-Regular"
        "Script-Regular"
        "Math-Italic"
        "Size1-Regular"
        "Size2-Regular"
        "Size3-Regular"
        "Size4-Regular"
        "Typewriter-Regular"
    ]

    let private fontfiles = [
        "fonts/KaTeX_Main-Regular.ttf"
        "fonts/KaTeX_AMS-Regular.ttf"
        "fonts/KaTeX_Caligraphic-Regular.ttf"
        "fonts/KaTeX_Fraktur-Regular.ttf"
        "fonts/KaTeX_SansSerif-Regular.ttf"
        "fonts/KaTeX_Script-Regular.ttf"
        "fonts/KaTeX_Math-Italic.ttf"
        "fonts/KaTeX_Size1-Regular.ttf"
        "fonts/KaTeX_Size2-Regular.ttf"
        "fonts/KaTeX_Size3-Regular.ttf"
        "fonts/KaTeX_Size4-Regular.ttf"
        "fonts/KaTeX_Typewriter-Regular.ttf"
    ]

    let private typefaces = [|for x in fontfiles -> SKTypeface.FromFile x|]

    let [<Literal>] fontsize = 18f

    // let WDTypefaces () =
    //     for typeface in typefaces do
    //         Console.WriteLine typeface.FamilyName

    [<Struct>]
    type Atom = {
        x: float32
        y: float32
        s: float32
        str: string
    }

    let inline transform x y (size:Vector4) :SKPoint =
        let xt = abs size.X + x
        let yt = size.Z - size.Y - y
        SKPoint(xt, yt) 
    
    // let rec printatom n padding (size:Vector4) =
    //     let pt = transform x y size
    //     let str = $"{(string n, -20)},  pos: X= {pt.X, -9}, Y= {pt.Y}"
    //     Console.WriteLine $"{padding}"

    // let printatoms ()

    let rec printExprs (exprs:seq<Expr>) (indent:string) =
        printf "%s" indent
        for expr in exprs do
            match expr with
            | Number _
            | Identifier _
            | MathOperator _ -> printf "%s" (string expr)
            | Symbol (t,s) -> printf "%s" s 
            | Binary (s,l,r) -> 
                printExprs [l] (indent)
                printExprs [r] (indent)
            | Grouped g -> 
                printfn ""
                printExprs g (indent + "--")
            | Up (e,u) ->
                printExprs [u] (indent + "--")
            | Down (e,d) ->
                printExprs [d] (indent + "--")
            | _ -> failwith $"(string expr) not implemented yet"
            

    let rec measure (exprs:seq<Expr>) (paint:SKPaint) (s:float32) :struct(float32 * float32) =
        let mutable w = 0f
        let mutable h = fontsize * s
        for expr in exprs do
            match expr with
            | Number n -> 
                w <- w + paint.MeasureText(n) * s
                h <- max h paint.TextSize * s
            | Identifier i ->
                w <- w + paint.MeasureText(i) * s
                h <- max h paint.TextSize * s
            | MathOperator o ->
                w <- w + paint.MeasureText(o) * s
                h <- max h paint.TextSize * s
            | Symbol (t,n) -> 
                w <- w + paint.MeasureText(n) * s
                h <- max h paint.TextSize * s
            | Binary (n,l,r) ->
                let struct(uw,uh) = measure [l] paint s
                let struct(lw,lh) = measure [r] paint s
                w <- w + (max uw lw)
                h <- h + (uh + lh)
            | Grouped g ->
                let struct(gw,gh) = measure g paint s
                w <- w + gw
                h <- max h gh                
            | Up (e,u) ->
                let struct(uw,uh) = measure [u] paint (0.8f * s)
                w <- w + uw
                h <- h + uh
            | Down (e,d) -> 
                let struct(lw,lh) = measure [d] paint (0.8f * s)
                w <- w + lw
                h <- h + lh
            | RightBrace -> w <- 8f 
            | _ -> failwith $"{string expr} is not implemented"
        struct(w,h)


    /// draws t with appropriate Typeface    
    let draw (t:string) x y s (paint:SKPaint) (canvas:SKCanvas) =
        let mutable break' = false
        let mutable i = 0
        while i < typefaces.Length && not break' do
            if typefaces[i].ContainsGlyphs(t) then
                paint.Typeface <- typefaces[i]
                paint.TextSize <- fontsize * s
                canvas.DrawText(t, x, y, paint)    
                // Console.WriteLine("{0},  {1}", typefaces[i].FamilyName, t)
                break' <- true
            i <- i + 1
        if not break' then
            canvas.DrawText(t, x, y, paint)
        paint.TextSize <- fontsize
        paint.Typeface <- typefaces[0]

    let dx (t:string) s (paint:SKPaint) =
        paint.MeasureText(t) * s

    let dy (t:string) s (paint:SKPaint) =
        paint.TextSize * s

    let rec typeset (exprs:seq<Expr>) x0 y0 (paint:SKPaint) (canvas:SKCanvas) (s:float32) =
        let mutable x = x0
        let mutable y = y0
        for expr in exprs do
            match expr with
            | Number n -> 
                draw n x y s paint canvas
                x <- x + dx n s paint
            | Identifier i ->
                draw i x y s paint canvas
                x <- x + dx i s paint
            | MathOperator o ->
                draw o x y s paint canvas
                x <- x + dx o s paint
            | Symbol (t,n) ->
                draw n x y s paint canvas
                x <- x + dx n s paint
            | Binary (n,l,r) ->
                let struct(uw,uh) = measure [l] paint s 
                let struct(lw,lh) = measure [r] paint s
                typeset [l] x (y - fontsize * s) paint canvas s
                typeset [r] x (y + fontsize * s) paint canvas s
                canvas.DrawLine(SKPoint(x,y), SKPoint(x + (max uw lw),y), paint)
                x <- x + max uw lw                
            | Grouped g ->
                let struct(w,h) = measure g paint s
                typeset g x y paint canvas s
                x <- x + w
            | Up (e,u) ->
                let struct(w,h) = measure [u] paint (0.8f * s)
                typeset [u] x (y - fontsize / 3f) paint canvas (0.8f * s)
                x <- x + w
            | Down (e,d) ->
                let struct(w,h) = measure [d] paint (0.8f * s)
                typeset [d] x (y + fontsize / 3f) paint canvas (0.8f * s)
                x <- x + w
            | RightBrace ->
                draw "}" x y s paint canvas
                x <- x + 8f
            | _ -> failwith $"{string expr} is not implemented yet"
            
        
    let render (exprs:seq<Expr>) (stream:System.IO.Stream) =
        use paint = new SKPaint(
            Typeface = typefaces[0],
            Color = SKColors.Black,
            IsAntialias = true,
            TextSize = fontsize,
            StrokeWidth = 2f
        )
        let struct(w,h) = measure exprs paint 1f
        let info = new SKImageInfo(int (w + 2f * 12f), int (h + 2f * 12f))
        use surface = SKSurface.Create(info)
        use canvas = surface.Canvas
        typeset exprs 30f 30f paint canvas 1f
        use image = surface.Snapshot()
        use data = image.Encode(SKEncodedImageFormat.Png, 80)
        data.SaveTo(stream)
