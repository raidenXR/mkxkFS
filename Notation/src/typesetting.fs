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

    let [<Literal>] binoffset = 4f

    [<Struct>]
    type Size = {
        w: float32 
        h: float32
    }  

    [<Struct>]
    type HBox = {
        dy1: float32 
        dy2: float32
    }
    
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
            | Over (e,o) -> 
                printExprs [o] (indent + "--")
            | Under (e,u) -> 
                printExprs [u] (indent + "--")
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
                let h = lhs_size.h + rhs_size.h + 0.5f * fontsize + binoffset
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
                let u_size = size u (0.8f * s)
                u_size
            | Down (e,d) ->
                let d_size = size d (0.8f * s)
                d_size
            | Downup (e,u,d) ->
                let u_size = size u (0.8f * s)
                let d_size = size d (0.8f * s)
                let w = max u_size.w d_size.w
                let h = u_size.h + d_size.h
                {w = w; h = h}
            | Over (e,o) ->
                let e_size = size e s
                {w = e_size.w; h = e_size.h + 0.2f * s * fontsize}
            | Under (e,u) ->
                let e_size = size e s
                {w = e_size.w; h = e_size.h + 0.2f * s * fontsize}
            | Underover _ ->
                failwith "Over, under, underover not implemented yet"
            | Scaled (str,e) ->
                failwith "scaled expr not implemented yet"
            | _ -> failwith $"{string expr} is not implemented yet"


        let totalSize (exprs:seq<Expr>) :Size =
            let mutable w = 0f
            let mutable h = 0f
            for expr in exprs do
                let s = size expr 1f
                w <- w + s.w
                h <- max h s.h
            {w = w; h = h}


        let rec hbox (expr:Expr) s :HBox =
            match expr with
            | Number _ 
            | Identifier _ 
            | MathOperator _ 
            | Symbol _ -> {dy1 = 0f; dy2 = 0f}
            | Binary (n,l,r) ->
                let lhs_size = size l s
                let rhs_size = size r s
                {dy1 = lhs_size.h; dy2 = -rhs_size.h}
            | Grouped group ->
                let mutable dy1 = 0f
                let mutable dy2 = 0f
                for g in group do
                    let hb = hbox g s
                    dy1 <- max dy1 hb.dy1
                    dy2 <- min dy2 hb.dy2
                {dy1 = dy1; dy2 = dy2}
            | Up (e,u) ->
                let u_size = size u (0.8f * s)
                {dy1 = u_size.h * 0.3f; dy2 = 0f}
            | Down (e,d) ->
                let d_size = size d (0.8f * s)
                {dy1 = 0f; dy2 = -d_size.h * 0.3f}
            | Over (e,o) ->
                let hb = hbox e s
                {dy1 = hb.dy1 + 0.2f * s * fontsize; dy2 = hb.dy2}
            | Under (e,u) ->
                let hb = hbox e s
                {dy1 = hb.dy1; dy2 = hb.dy2 + 0.2f * s * fontsize}
            | Scaled _ ->
                failwith "not implemented yet"
            | _ -> failwith $"{string expr} is not implemented yet"                    


        let totalHbox (exprs:seq<Expr>) :HBox =
            let mutable dy1 = 0f
            let mutable dy2 = 0f
            for expr in exprs do
                let hb = hbox expr 1f            
                dy1 <- max dy1 hb.dy1
                dy2 <- min dy2 hb.dy2
            {dy1 = dy1; dy2 = dy2}
                

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
            draw regular n pt.X pt.Y s canvas r
            pt.X <- pt.X + s * Measure.width n
        | Binary (n,lhs,rhs) ->
            let s = 0.95f * s
            let lhs_size = Measure.size lhs s
            let rhs_size = Measure.size rhs s
            let w = max lhs_size.w rhs_size.w
            let x0 = if lhs_size.w > 0.9f * w then pt.X else pt.X + (w - lhs_size.w) / 2f
            let x1 = if rhs_size.w > 0.9f * w then pt.X else pt.X + (w - rhs_size.w) / 2f

            let lhs_hbox = Measure.hbox lhs s
            let rhs_hbox = Measure.hbox rhs s
            let y_line = pt.Y + 0.5f * fontsize
            let y0 = y_line + abs (lhs_hbox.dy2) + binoffset
            let y1 = y_line - s * fontsize - abs (rhs_hbox.dy1)

            let mutable pt0 = Vector2(x0, y0)
            let mutable pt1 = Vector2(x1, y1)
            canvas.DrawLine((transform pt.X y_line r), (transform (pt.X + w) y_line r), paint)
            typeset lhs &pt0 s canvas r
            typeset rhs &pt1 s canvas r
            pt.X <- pt.X + w
        | Grouped group ->
            let gsize = Measure.totalSize group
            for g in group do
                let g_size = Measure.size g s
                typeset g &pt s canvas r
        | Up (e,u) ->
            let e_size = Measure.size e s
            let u_size = Measure.size u (0.8f * s)
            let mutable pt0 = Vector2(pt.X, pt.Y + 0.6f * e_size.h)
            typeset u &pt0 (0.8f * s) canvas r  
            pt.X <- pt.X + u_size.w
        | Down (e,d) ->
            let e_size = Measure.size e s
            let d_size = Measure.size d (0.8f * s)
            let mutable pt0 = Vector2(pt.X, pt.Y - 0.3f * d_size.h)
            typeset d &pt0 (0.8f * s) canvas r  
            pt.X <- pt.X + d_size.w
        // | Downup _
        | Over (e,o) ->
            let mutable pt0 = Vector2(pt.X, pt.Y + binoffset)
            typeset o &pt0 s canvas r
            typeset e &pt s canvas r
        | Under (e,u) ->
            let mutable pt0 = Vector2(pt.X, pt.Y - binoffset)
            typeset u &pt0 s canvas r
            typeset e &pt s canvas r
        | Scaled _ ->
            failwith "not implemented yet"
        | _ -> failwith $"{string expr} is not implemented yet"
                           
        
    let render (transparent:bool) (stream:System.IO.Stream) (exprs:seq<Expr>) =
        let total_size = Measure.totalSize exprs
        let total_hbox = Measure.totalHbox exprs
        let w = int (total_size.w + 2f * fontsize)
        let h = int (total_size.h + 2f * fontsize)
        let info = new SKImageInfo(w, h)

        use surface = SKSurface.Create(info)
        use canvas = surface.Canvas
        if not transparent then canvas.Clear(SKColors.White)

        let mutable pos = Vector2(fontsize, abs (total_hbox.dy2) - fontsize)
        for expr in exprs do
            typeset expr &pos 1f canvas total_size
        use image = surface.Snapshot()
        use data = image.Encode(SKEncodedImageFormat.Png, 80)
        data.SaveTo(stream)

    /// renders image with transparent background and no margin
    let renderAlpha (stream:System.IO.Stream) (exprs:seq<Expr>) =
        let total_size = Measure.totalSize exprs
        let total_hbox = Measure.totalHbox exprs
        let w = int (total_size.w + fontsize)
        let h = int (total_size.h + fontsize)
        let info = new SKImageInfo(w, h)

        use surface = SKSurface.Create(info)
        use canvas = surface.Canvas

        let mutable pos = Vector2(0f, 0f)
        for expr in exprs do
            typeset expr &pos 1f canvas total_size
        use image = surface.Snapshot()
        use data = image.Encode(SKEncodedImageFormat.Png, 80)
        data.SaveTo(stream)

    /// renders an error image in case parsing fails
    let errorImg (stream:System.IO.Stream) =
        let info = new SKImageInfo(320,60)
        use surface = SKSurface.Create(info)
        use canvas = surface.Canvas
        paint.Color <- SKColors.Red
        canvas.DrawText("ERROR in parsing exprs", SKPoint(5f,55f), paint)
        paint.Color <- SKColors.Black
        use image = surface.Snapshot()
        use data = image.Encode(SKEncodedImageFormat.Png, 80)
        data.SaveTo(stream)
