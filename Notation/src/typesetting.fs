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

    [<Struct>]
    type HBox = {
        xmin: float32
        xmax: float32
        ymin: float32
        ymax: float32
    }
    with
        member r.W = abs (r.xmax - r.xmin)

        member r.H = abs (r.ymax - r.ymin)

        member r.Size = {w = abs (r.xmax - r.xmin); h = abs (r.ymax - r.ymin)}

        static member (+) (a:HBox, b:HBox) = {
                xmin = min a.xmin b.xmin 
                xmax = max a.xmax b.xmax
                ymin = min a.ymin b.ymin
                ymax = max a.ymax b.ymax
            }

    // let inline transform x y (size:Rect) :SKPoint =
    //     let xt = abs size.x + x
    //     let yt = size.h - size.y - y
    //     SKPoint(xt, yt) 
    
    let inline transform x y (size:Size) :SKPoint =
        let xt = x
        let yt = size.h - y
        SKPoint(xt, yt)
    
    // let rec printatom n padding (size:Vector4) =
    //     let pt = transform x y size
    //     let str = $"{(string n, -20)},  pos: X= {pt.X, -9}, Y= {pt.Y}"
    //     Console.WriteLine $"{padding}"

    // let printatoms ()

    
    let private paint = new SKPaint(
        Typeface = regular[0],
        Color = SKColors.Black,
        IsAntialias = true,
        TextSize = fontsize,
        StrokeWidth = 1.3f
    )

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


    let rec measureHBox (expr:Expr) x y s :HBox = 
        match expr with
        | Number n ->
            paint.Typeface <- regular[1]
            {xmin = x; xmax = x + s * paint.MeasureText n; ymin = y; ymax = y + fontsize * s}
        | Identifier i ->
            paint.Typeface <- italic[0]
            {xmin = x; xmax = x + s * paint.MeasureText i; ymin = y; ymax = y + fontsize * s}
        | MathOperator o ->
            paint.Typeface <- italic[1]
            {xmin = x; xmax = x + s * paint.MeasureText o; ymin = y; ymax = y + fontsize * s}
        | Symbol (t,n) ->
            paint.Typeface <- regular[1]
            {xmin = x; xmax = x + s * paint.MeasureText n; ymin = y; ymax = y + fontsize * s}
        | Binary (n,l,r) ->
            let s = 0.95f * s
            let y_line = y + s * fontsize / 2f
            let y_lhs = y_line + s * fontsize
            let y_rhs = y_line - s * fontsize
            let lhs = measureHBox l x y_lhs s
            let rhs = measureHBox r x y_rhs s
            // let y_upper = y_lhs + if lhs.ymin < y_line then abs (lhs.ymin - y_line) else 0f
            // let y_lower = y_rhs + if rhs.ymax > y_line then - abs (rhs.ymax - y_line) else 0f
            // let lhs' = measureHBox l paint x y_upper s
            // let rhs' = measureHBox l paint x y_lower s
            // lhs' + rhs'       
            lhs + rhs
        | Grouped group ->
            let mutable hbox = {xmin = x; xmax = x; ymin = y; ymax = y}
            for g in group do
                hbox <- hbox + measureHBox g hbox.xmax y s 
            hbox
        | Up (e,u) ->
            let e_hbox = measureHBox e x y s
            let yu = e_hbox.ymax - fontsize  / 2f
            measureHBox u x yu (0.8f * s)
        | Down (e,d) ->
            let e_hbox = measureHBox e x y s                
            let yd = e_hbox.ymin - fontsize / 3f
            measureHBox d x yd (0.8f * s)
        | _ -> failwith $"{string expr} is not implemented"


    let rec measure (exprs:seq<Expr>) x y s :HBox =
        let mutable hbox = {xmin = x; xmax = x; ymin = y; ymax = y}
        for expr in exprs do
            hbox <- hbox + measureHBox expr hbox.xmax y s
        hbox        


    let rec measureWithPrint (exprs:seq<Expr>) (paint:SKPaint) x y s :unit = 
        let mutable hbox = {xmin = 0f; xmax = 0f; ymin = 0f; ymax = 0f}
        for expr in exprs do
            hbox <- hbox + measureHBox expr hbox.xmax y s
            printfn "%s  xmin:%f, xmax:%f, ymin:%f, ynax:%f" (string expr) hbox.xmin hbox.xmax hbox.ymin hbox.ymax        


    /// draws t with appropriate Typeface    
    let rec draw (typefaces:array<SKTypeface>) (t:string) x y s (paint:SKPaint) (canvas:SKCanvas) (r:Size) =
        let mutable break' = false
        let mutable i = 0
        match (typefaces |> Array.tryFind (fun x -> x.ContainsGlyphs t)) with
        | Some typeface -> 
            paint.Typeface <- typeface
            paint.TextSize <- fontsize * s
            let pt = transform x y r
            canvas.DrawText(t, pt, paint)
        | _ -> draw allfonts t x y s paint canvas r
        paint.TextSize <- fontsize


    let dx (t:string) s (paint:SKPaint) = paint.MeasureText(t) * s

    let dy (t:string) s (paint:SKPaint) = paint.TextSize * s

    /// replace with typeset for singleHBox
    let rec typesetHBox (expr:Expr) (x:byref<float32>) (y:byref<float32>) (paint:SKPaint) (canvas:SKCanvas) s size :unit =
        match expr with
        | Number n ->
            let hbox = measureHBox expr x y s
            draw regular n x y s paint canvas size
            x <- x + hbox.W
        | Identifier i ->
            let hbox = measureHBox expr x y s
            draw italic i x y s paint canvas size
            x <- x + hbox.W
        | MathOperator o ->
            let hbox = measureHBox expr x y s
            draw regular o x y s paint canvas size
            x <- x + hbox.W
        | Symbol (t,n) ->
            let hbox = measureHBox expr x y s
            draw italic n x y s paint canvas size
            x <- x + hbox.W
        | Binary (n,l,r) ->
            let s = 0.95f * s
            let y_line = y + s * fontsize / 2f
            let y_lhs = y_line + s * fontsize
            let y_rhs = y_line - s * fontsize
            let lhs = measureHBox l x y_lhs s
            let rhs = measureHBox r x y_rhs s
            let width = max lhs.W rhs.W
            let mutable x_lhs = if lhs.W > 0.9f * width then x else x + (width - lhs.W) / 2f
            let mutable x_rhs = if rhs.W > 0.9f * width then x else x + (width - rhs.W) / 2f
            let mutable y_upper = y_lhs + if lhs.ymin < y_line then abs (lhs.ymin - y_line) else 0f
            let mutable y_lower = y_rhs + if rhs.ymax > y_line then - abs (rhs.ymax - y_line) else 0f
            typesetHBox l &x_lhs &y_upper paint canvas s size
            typesetHBox r &x_rhs &y_lower paint canvas s size
            let pt0 = transform x y_line size
            let pt1 = transform (x + width) y_line size
            canvas.DrawLine(pt0, pt1, paint)
            x <- x + width                
        | Grouped group ->
            // let g_hbox = measure group paint x y s
            // let mutable x' = x
            // let mutable y' = y
            for g in group do
                typesetHBox g &x &y paint canvas s size
                // typesetHBox g &x' &y' paint canvas s size
            // x <- x'
        | Up (e,u) ->
            let e_hbox = measureHBox e x y s
            let mutable yu = e_hbox.ymax - fontsize  / 2f
            let u_hbox = measureHBox u x yu (0.8f * s)
            typesetHBox u &x &yu paint canvas (0.8f * s) size
            x <- x + u_hbox.W
        | Down (e,d) ->
            let e_hbox = measureHBox e x y s                
            let mutable yd = e_hbox.ymin - fontsize / 3f
            let d_hbox = measureHBox d x yd (0.8f * s)
            typesetHBox d &x &yd paint canvas (0.8f * s) size
            x <- x + d_hbox.W
        | _ -> failwith $"{string expr} is not implemented yet"


    let rec typeset (exprs:seq<Expr>) (paint:SKPaint) (canvas:SKCanvas) s size :unit = 
        let mutable x = 0f
        let mutable y = 0f
        for expr in exprs do
            typesetHBox expr &x &y paint canvas s size
                           
        
    
    let render (exprs:seq<Expr>) (stream:System.IO.Stream) =
        let hbox = measure exprs 0f 0f 1f
        let size = hbox.Size
        let info = new SKImageInfo(int size.w, 124 + int size.h)
        use surface = SKSurface.Create(info)
        use canvas = surface.Canvas
        canvas.Clear(SKColors.White)
        typeset exprs paint canvas 1f size
        use image = surface.Snapshot()
        use data = image.Encode(SKEncodedImageFormat.Png, 80)
        data.SaveTo(stream)
