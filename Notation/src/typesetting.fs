namespace NotationFS
open System
open System.Numerics
open SkiaSharp

module Typesetting =

    let private regular = Array.map SKTypeface.FromFile [|
        "fonts/KaTeX_AMS-Regular.ttf"
        "fonts/KaTeX_Main-Regular.ttf"
        "fonts/KaTeX_Size1-Regular.ttf"
        "fonts/KaTeX_Size2-Regular.ttf"
        "fonts/KaTeX_Size3-Regular.ttf"
        "fonts/KaTeX_Size4-Regular.ttf"
        "fonts/KaTeX_Script-Regular.ttf"
        "fonts/KaTeX_Fraktur-Regular.ttf"
        "fonts/KaTeX_SansSerif-Regular.ttf"
        "fonts/KaTeX_Typewriter-Regular.ttf"        
        "fonts/KaTeX_Caligraphic-Regular.ttf"
    |]

    let private italic = Array.map SKTypeface.FromFile [|
        "fonts/KaTeX_Main-Italic.ttf"
        "fonts/KaTeX_Math-Italic.ttf"
        "fonts/KaTeX_SansSerif-Italic.ttf"
    |]

    let private bold = Array.map SKTypeface.FromFile [|
        "fonts/KaTeX_Main-Bold.ttf"        
        "fonts/KaTeX_Fraktur-Bold.ttf"
        "fonts/KaTeX_SansSerif-Bold.ttf"
        "fonts/KaTeX_Caligraphic-Bold.ttf"        
    |]

    let private bolditalic = Array.map SKTypeface.FromFile [|
        "fonts/KaTeX_Main-BoldItalic.ttf"
        "fonts/KaTeX_Math-BoldItalic.ttf"
    |]

    let private allfonts = Array.concat [regular; italic; bold; bolditalic]
    
    let [<Literal>] fontsize = 18f

    [<Struct>]
    type Atom = {
        x: float32
        y: float32
        s: float32
        str: string
    }

    [<Struct>]
    type Size = {
        w: float32
        h: float32
    }

    [<Struct>]
    type Rect = {
        x: float32
        y: float32
        w: float32
        h: float32
        ymin: float32
        ymax: float32
    }

    let inline transform x y (size:Rect) :SKPoint =
        let xt = abs size.x + x
        let yt = size.h - size.y - y
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
            

    let rec measure (exprs:seq<Expr>) (paint:SKPaint) x y s :Rect =
        let mutable w = 0.f
        let mutable h = fontsize * s
        let mutable ymin = y
        let mutable ymax = y
        for expr in exprs do
            match expr with
            | Number n -> 
                paint.Typeface <- regular[1]
                w <- w + paint.MeasureText(n) * s
                h <- max h (y + (paint.TextSize * s))
            | Identifier i ->
                paint.Typeface <- italic[0]
                w <- w + paint.MeasureText(i) * s
                h <- max h (y + (paint.TextSize * s))
            | MathOperator o ->
                paint.Typeface <- italic[1]
                w <- w + paint.MeasureText(o) * s
                h <- max h (y + (paint.TextSize * s))
            | Symbol (t,n) -> 
                paint.Typeface <- regular[1]
                w <- w + paint.MeasureText(n) * s
                h <- max h (y + (paint.TextSize * s))
            | Binary (n,l,r) ->
                let ls = measure [l] paint x y s
                let rs = measure [r] paint x y s
                let bw = max ls.w rs.w
                w <- w + bw
                h <- max h (ls.y + ls.h + rs.y + rs.h)
                ymax <- ls.ymax
                ymin <- rs.ymin
            | Grouped g ->
                let gs = measure g paint x y s
                w <- w + gs.w
                h <- max h gs.h                
            | Up (e,u) ->
                let us = measure [u] paint x y (0.8f * s)
                w <- w + us.w
                ymax <- ymax + fontsize / 3f
                h <- max h (ymax + 0.8f * s * fontsize)
            | Down (e,d) -> 
                let ls = measure [d] paint x y (0.8f * s)
                w <- w + ls.w
                ymin <- ymin - fontsize / 3f
                h <- max h (ymin + 0.8f * fontsize)
            | RightBrace -> w <- 8f 
            | _ -> failwith $"{string expr} is not implemented"
        paint.Typeface <- regular[1]
        {x = x; y = y; w = w; h = h; ymin = ymin; ymax = ymax}


    /// draws t with appropriate Typeface    
    let rec draw (typefaces:array<SKTypeface>) (t:string) x y s (paint:SKPaint) (canvas:SKCanvas) (r:Rect) =
        let mutable break' = false
        let mutable i = 0
        while i < typefaces.Length && not break' do
            if typefaces[i].ContainsGlyphs(t) then
                paint.Typeface <- typefaces[i]
                paint.TextSize <- fontsize * s
                let pt = transform x y r
                canvas.DrawText(t, pt, paint)    
                break' <- true
            i <- i + 1
        if not break' then
            draw allfonts t x y s paint canvas r
        paint.TextSize <- fontsize


    let dx (t:string) s (paint:SKPaint) = paint.MeasureText(t) * s

    let dy (t:string) s (paint:SKPaint) = paint.TextSize * s

    let rec typeset (exprs:seq<Expr>) x0 y0 (paint:SKPaint) (canvas:SKCanvas) (s:float32) (r:Rect) =
        let mutable x = x0
        let mutable y = y0
        for expr in exprs do
            match expr with
            | Number n -> 
                draw regular n x y s paint canvas r
                x <- x + dx n s paint
            | Identifier i ->
                draw italic i x y s paint canvas r
                x <- x + dx i s paint
            | MathOperator o ->
                draw regular o x y s paint canvas r
                x <- x + dx o s paint
            | Symbol (t,n) ->
                draw regular n x y s paint canvas r
                x <- x + dx n s paint
            | Binary (n,lhs,rhs) ->
                let us = measure [lhs] paint x y s 
                let ls = measure [rhs] paint x y s
                let lw = max us.w ls.w
                let ux = if us.w > 0.9f * lw then x else x + (lw - us.w) / 2.f
                let lx = if ls.w > 0.9f * lw then x else x + (lw - ls.w) / 2.f                
                let uy = y + us.h / 2f
                let ly = y - ls.h / 2f
                typeset [lhs] ux uy paint canvas s r
                typeset [rhs] lx ly paint canvas s r
                let pt0 = transform x y r 
                let pt1 = transform (x + lw) y r
                canvas.DrawLine(pt0, pt1, paint)
                x <- x + max us.w ls.w                
            | Grouped g ->
                typeset g x y paint canvas s r
                let gs = measure g paint x y s
                x <- x + gs.w
            | Up (e,u) ->
                typeset [u] x (y + fontsize / 3f) paint canvas (0.8f * s) r
                let us = measure [u] paint x y (0.8f * s)
                x <- x + us.w
            | Down (e,d) ->
                let ds = measure [d] paint x0 y0 (0.8f * s) 
                typeset [d] x (y - fontsize / 3f) paint canvas (0.8f * s) r
                x <- x + ds.w
            | RightBrace ->
                // draw regular "}" x y s paint canvas
                x <- x + 8f
            | _ -> failwith $"{string expr} is not implemented yet"
            
        
    let render (exprs:seq<Expr>) (stream:System.IO.Stream) =
        use paint = new SKPaint(
            Typeface = regular[0],
            Color = SKColors.Black,
            IsAntialias = true,
            TextSize = fontsize,
            StrokeWidth = 1.3f
        )
        let size = measure exprs paint 0f 0f 1f
        let info = new SKImageInfo(int (size.w + 2f * 12f), int (size.h + 2f * 12f))
        use surface = SKSurface.Create(info)
        use canvas = surface.Canvas
        typeset exprs 0f 0f paint canvas 1f size
        use image = surface.Snapshot()
        use data = image.Encode(SKEncodedImageFormat.Png, 80)
        data.SaveTo(stream)
