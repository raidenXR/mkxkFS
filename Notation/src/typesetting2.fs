namespace NotationFS2
open System
open System.Numerics
open SkiaSharp
open NotationFS

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

    type [<Struct>] Size = {w:float32; h:float32}

    type Position = Vector2
    
    type [<Struct>] HBox = {pos:Vector2; size:Size; s:float32}
    with
        member r.xmax = r.pos.X + r.size.w
        member r.ymax = r.pos.Y + r.size.h

    
    let inline private transform x y (size:Size) = SKPoint(x, size.h - y)
    

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



    // simple standard paint to measure sizes
    let private paint = new SKPaint(
        Color = SKColors.Black,
        StrokeWidth = 1.3f,
        IsAntialias = true,
        TextSize = fontsize,
        Typeface = regular[1]
    )

    let rec private selectfont (fonts:array<SKTypeface>) (str:string) =
        match (fonts |> Array.tryFind (fun x -> x.ContainsGlyphs str)) with
        | Some typeface -> typeface
        | None -> selectfont allfonts str 

    
    module Measure =

        let width (str:string) = paint.MeasureText str
    
        /// s: scale
        let rec size (expr:Expr) (s:float32) :Size =
            match expr with
            | Number n -> 
                paint.Typeface <- regular[1]
                let w = s * width n
                let h = s * fontsize
                {w = w; h = h}
            | Identifier n ->
                paint.Typeface <- selectfont italic n
                let w = s * width n
                let h = s * fontsize
                {w = w; h = h}
            | MathOperator n ->
                paint.Typeface <- selectfont regular n
                let w = s * width  n
                let h = s * fontsize
                {w = w; h = h}
            | Symbol (t,n) ->
                paint.Typeface <- selectfont italic n
                let w = s * width n
                let h = s * fontsize
                {w = w; h = h}
            | Binary (n,lhs,rhs) ->
                let s = 0.92f * s
                let lhs_size = size lhs s
                let rhs_size = size rhs s
                let w = max lhs_size.w rhs_size.w
                let h = lhs_size.h + rhs_size.h
                {w = w; h = h}
            | Grouped group ->
                let mutable w = 0f
                let mutable h = 0f
                for g in group do
                    let g_size = size g s
                    w <- w + g_size.w
                    h <- max h g_size.h
                {w = w; h = h}
            | Up (e,u) ->
                // let e_size = size e s
                let u_size = size u (0.8f * s)
                u_size
            | Down (e,d) ->
                // let e_size = size e s
                let d_size = size d (0.8f * s)
                d_size
            | _ -> failwith $"{string expr} is not implemented yet"


        let totalSize (exprs:seq<Expr>) :Size =
            let mutable w = 0f
            let mutable h = 0f
            for expr in exprs do
                let s = size expr 1f
                w <- w + s.w
                h <- max h s.h
            {w = w; h = h}

        [<Obsolete>]
        let rec hbox (expr:Expr) x y s :HBox =
            match expr with 
            | Number n -> 
                let pos = Vector2(x, y)
                let size = size expr s
                {pos = pos; size = size; s = s}
            | Identifier n ->
                let pos = Vector2(x, y)
                let size = size expr s
                {pos = pos; size = size; s = s}
            | MathOperator n ->    
                let pos = Vector2(x, y)
                let size = size expr s
                {pos = pos; size = size; s = s}
            | Symbol (t,n) -> 
                let pos = Vector2(x, y)
                let size = size expr s
                {pos = pos; size = size; s = s}
            | Binary (n,lhs,rhs) ->
                let lhs_size = size lhs (0.95f * s)
                let rhs_size = size rhs (0.95f * s)
                let y_line = y + 0.5f * fontsize

                let w = max lhs_size.w rhs_size.w
                let x0 = if lhs_size.w > 0.9f * w then x else x + (w - lhs_size.w) / 2f
                let x1 = if rhs_size.w > 0.9f * w then x else x + (w - rhs_size.w) / 2f

                let y0 = y_line
                let y1 = y_line - rhs_size.h

                let b_size = {w = w; h = lhs_size.h + rhs_size.h}
                {pos = Vector2(x, y_line); size = b_size; s = s}
            | Grouped group ->
                let mutable w = x                    
                for g in group do
                    let gbox = hbox g w y s
                    w <- w + gbox.size.w
                {pos = Vector2(x, y); size = {w = w; h = y}; s = s}
                    
            | _ -> failwith $"{string expr} is not implemented yet"


    /// draws str with appropriate Typeface    
    let draw (fonts:array<SKTypeface>) (str:string) x y s (canvas:SKCanvas) (r:Size) =
        paint.Typeface <- selectfont fonts str 
        paint.TextSize <- s * fontsize
        let pt = transform x y r
        canvas.DrawText(str, pt, paint)
        paint.TextSize <- fontsize


    let rec typeset (expr:Expr) (pt:byref<Vector2>) s canvas r :unit =
        match expr with 
        | Number n ->             
            draw regular n pt.X pt.Y s canvas r
            pt.X <- pt.X + s * Measure.width n
        | Identifier n ->
            draw italic n pt.X pt.Y s canvas r
            pt.X <- pt.X + s * Measure.width n
        | MathOperator n ->
            draw regular n pt.X pt.Y s canvas r
            pt.X <- pt.X + s * Measure.width n
        | Symbol (t,n) ->
            draw italic n pt.X pt.Y s canvas r
            pt.X <- pt.X + s * Measure.width n
        | Binary (n,lhs,rhs) ->
            let s = 0.95f * s
            let lhs_size = Measure.size lhs s
            let rhs_size = Measure.size rhs s

            let w = max lhs_size.w rhs_size.w
            let x0 = if lhs_size.w > 0.9f * w then pt.X else pt.X + (w - lhs_size.w) / 2f
            let x1 = if rhs_size.w > 0.9f * w then pt.X else pt.X + (w - rhs_size.w) / 2f

            let y0 = pt.Y + s * 1.5f * fontsize
            let y1 = pt.Y - s * 1.5f * fontsize
            let y_line = pt.Y + 0.5f * fontsize
            // let y_line = y1 + (y0 - y1) / 2f

            let mutable pt0 = Vector2(x0, y0)
            let mutable pt1 = Vector2(x1, y1)
            typeset lhs &pt0 s canvas r
            typeset rhs &pt1 s canvas r
            canvas.DrawLine((transform pt.X y_line r), (transform (pt.X + w) y_line r), paint)
        | Grouped group ->
            let mutable pt0 = Vector2(pt.X, pt.Y)
            let gsize = Measure.totalSize group
            for g in group do
                let g_size = Measure.size g s
                typeset g &pt0 s canvas r
                pt0.X <- pt0.X + g_size.w           
            pt.X <- pt.X + gsize.w
        | Up (e,u) ->
            let e_size = Measure.size e s
            let u_size = Measure.size u (0.8f * s)
            let mutable pt0 = Vector2(pt.X, pt.Y + 0.8f * e_size.h)
            typeset u &pt0 (0.8f * s) canvas r  
            pt.X <- pt.X + u_size.w
        | Down (e,d) ->
            let e_size = Measure.size e s
            let d_size = Measure.size d (0.8f * s)
            let mutable pt0 = Vector2(pt.X, pt.Y + e_size.h - 0.8f * e_size.h)
            typeset d &pt0 (0.8f * s) canvas r  
            pt.X <- pt.X + d_size.w
        | _ -> failwith $"{string expr} is not implemented yet"
                           
        
    let render (stream:System.IO.Stream) (exprs:seq<Expr>) =
        let total_size = Measure.totalSize exprs
        let w = int (total_size.w + 2f * fontsize)
        let h = int (total_size.h + 2f * fontsize)
        let info = new SKImageInfo(w, h)
        let mutable pos = Vector2.Zero

        use surface = SKSurface.Create(info)
        use canvas = surface.Canvas
        canvas.Clear(SKColors.White)
        for expr in exprs do
            typeset expr &pos 1f canvas total_size
        use image = surface.Snapshot()
        use data = image.Encode(SKEncodedImageFormat.Png, 80)
        data.SaveTo(stream)
