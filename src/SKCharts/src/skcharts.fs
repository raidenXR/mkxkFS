namespace SKCharts

open System
open System.Numerics
open SkiaSharp


type SKChart2(models':list<string * Model2>) =
    
    let mutable bounds = {xmin = 0.; xmax = 1.; ymin = 0.; ymax = 1.}

    let axis   = [|Vector2(0f,0f); Vector2(1f,0f); Vector2(0f,0f); Vector2(0f,1f)|]
    let grid   = Array.zeroCreate<Vector2> 20
    let ticks  = Array.zeroCreate<Vector2> 100 
    let labels = Array.zeroCreate<Vector2> 100   

    let axis_pts   = Array.zeroCreate<SKPoint> 4
    let grid_pts   = Array.zeroCreate<SKPoint> 20
    let tick_pts   = Array.zeroCreate<SKPoint> 100
    let label_pts  = Array.zeroCreate<SKPoint> 100
    let label_vals = Array.zeroCreate<float> 100 

    let mutable label_pts_slice = Memory<SKPoint>(label_pts)
    let mutable w = 800f
    let mutable h = 600f
    let mutable legend = SKPoint()
    let mutable ximg: SKImage = null
    let mutable yimg: SKImage = null
    let mutable xtitle = String.Empty
    let mutable ytitle = String.Empty
    let mutable is_disposed = false

    let models = System.Collections.Generic.List<Model2>()

    let paint_black = new SKPaint(
        Color = SKColors.Black, 
        StrokeWidth = 1.5f, 
        IsAntialias = true,
        TextSize = 16f
    )
    
    let paint_silver = new SKPaint(
        Color = SKColors.Silver, 
        StrokeWidth = 1.0f, 
        IsAntialias = true,
        TextSize = 16f        
    )
    
    let paint_model = new SKPaint(
        Color = SKColors.White, 
        StrokeWidth = 1.0f, 
        IsAntialias = true,
        TextSize = 16f        
    )

    let transform = 
        let translate = Matrix3x2.CreateTranslation(0.3f,0.4f)
        let scale     = Matrix3x2.CreateScale(0.6f,0.6f)
        translate * scale 

    let cast (vec:Vector2) = SKPoint(w * vec.X, h * (1f - vec.Y))        
        
    /// updates bounds, applies noramization and transformation
    let update_state a =
        ignore a 
        bounds <- if models.Count = 1 then (Bounds2.ofArray models[0].xvalues models[0].yvalues) else bounds
        for m in models do
            let mb = Bounds2.ofArray m.xvalues m.yvalues
            bounds <- Bounds2.compare mb bounds
        for m in models do 
            Model2.normalize bounds m
            Model2.transformPts transform w h m

    let add_model = models.Add >> update_state

    let remove_model =  models.Remove >> update_state

    let remove_model_at = models.RemoveAt >> update_state


    let update_gridlines () = 
        let mutable i = 0
        let mutable dv = 0.2f
        while i < 20 do
            grid_pts[i + 0] <- cast (Vector2.Transform(Vector2(dv, 0f), transform))
            grid_pts[i + 1] <- cast (Vector2.Transform(Vector2(dv, 1f), transform))
            grid_pts[i + 2] <- cast (Vector2.Transform(Vector2(0f, dv), transform))
            grid_pts[i + 3] <- cast (Vector2.Transform(Vector2(1f, dv), transform))
            dv <- dv + 0.2f
            i  <- i + 4


    let update_axes () =
        for i in 0..axis.Length - 1 do
            axis_pts[i] <- cast (Vector2.Transform(axis[i], transform))

    let update_ticks () = 
        let mutable i = 0
        let mutable dv = 0.2f
        while dv < 1.0f do
            tick_pts[i + 0] <- cast (Vector2.Transform(Vector2(dv, 0f), transform))
            tick_pts[i + 1] <- cast (Vector2.Transform(Vector2(dv, -0.05f), transform))
            tick_pts[i + 2] <- cast (Vector2.Transform(Vector2(0f, dv), transform))
            tick_pts[i + 3] <- cast (Vector2.Transform(Vector2(-0.05f, dv), transform))
            dv <- dv + 0.2f
            i  <- i + 4
        

    let update_labels () = 
        let mutable dv = 0.2f
        let mutable i = 0
        while dv < 1.0f do
            label_vals[i + 0] <- (float dv) * (bounds.xmax - bounds.xmin) + bounds.xmin
            label_vals[i + 1] <- (float dv) * (bounds.ymax - bounds.ymin) + bounds.ymin
            label_pts[i + 0]  <- cast (Vector2.Transform(Vector2(dv - 0.05f, -0.1f), transform))
            label_pts[i + 1]  <- cast (Vector2.Transform(Vector2(-0.1f - 0.05f, dv), transform))
            dv <- dv + 0.2f
            i  <- i + 2
        label_vals[i + 0] <- bounds.xmax
        label_vals[i + 1] <- bounds.ymax
        label_pts[i + 0]  <- cast (Vector2.Transform(Vector2(1.0f, -0.1f), transform))
        label_pts[i + 1]  <- cast (Vector2.Transform(Vector2(-0.1f, 1.0f), transform))
        i <- i + 2
        label_pts_slice <- Memory<SKPoint>(label_pts, 0, i)


    let update_models () =
        update_state ()
    

    let draw_labels (canvas:SKCanvas) = 
        let slice  = Span(label_pts)
        let len = label_pts.Length - 2

        for i in 0..2..len do
             canvas.DrawText(label_vals[i + 0].ToString("N3"), slice[i + 0].X, slice[i + 0].Y, paint_black)
             canvas.DrawText(label_vals[i + 1].ToString("N3"), slice[i + 1].X, slice[i + 1].Y, paint_black)

        if ximg <> null then
            canvas.DrawImage(ximg, slice[len + 0].X, slice[len + 0].Y, paint_black)
        else 
            canvas.DrawText(xtitle, slice[len + 0].X, slice[len + 0].Y, paint_black)
        
        if yimg <> null then
            canvas.DrawImage(yimg, slice[len + 1].X, slice[len + 1].Y, paint_black)
        else 
            canvas.DrawText(ytitle, slice[len + 1].X, slice[len + 1].Y, paint_black)

            

    let draw_legend (canvas:SKCanvas) = 
        let mutable vec = legend
        for m in models do
            paint_model.Color <- m.color
            canvas.DrawCircle(SKPoint(vec.X, vec.Y), 5f, paint_model)
            canvas.DrawText(m.name, SKPoint(vec.X + 10f, vec.Y), paint_model)
            vec.Y <- vec.Y + 20f


    let draw_models (canvas:SKCanvas) =
        for m in models do
            paint_model.Color <- m.color
            match m.kind with
            | Line -> 
                canvas.DrawPoints(SKPointMode.Lines, m.points, paint_model)
            | Points ->
                canvas.DrawPoints(SKPointMode.Points, m.points, paint_model)
            | _ -> failwith "Kind not implemented, in draw_models"


    do 
        models' |> List.iter (fun (name,model) -> models.Add({model with name = name}))
        update_state ()


    interface IDisposable with 
        member this.Dispose() = 
            if not is_disposed then
                paint_black.Dispose()
                paint_silver.Dispose()
                paint_model.Dispose()
            is_disposed <- true

    member this.W
        with get() = w
        and set(value) = w <- value
        
    member this.H 
        with get() = h
        and set(value) = h <- value

    member this.XTitle 
        with get() = xtitle
        and set(value) = xtitle <- value
        
    member this.YTitle 
        with get() = ytitle
        and set(value) = ytitle <- value

    member this.XImg 
        with get() = ximg
        and set(value) = ximg <- value

    member this.YImg 
        with get() = yimg
        and set(value) = yimg <- value

    member this.AddModel(model:Model2) = add_model model

    member this.RemoveModel(model:Model2) = remove_model model

    member this.RemoveModelAt(idx:int) = remove_model_at idx

    
    member this.UpdateModels () = 
        update_models ()        
    
    
    member this.Update() =
        update_gridlines ()
        update_axes ()
        update_ticks ()
        update_models ()
        update_labels ()

        let t = Matrix3x2.CreateTranslation(0.6f, 0.6f) * Matrix3x2.CreateScale(0.5f, 0.5f)
        let p = Vector2.Transform(Vector2(1f, 0.8f), t)
        legend <- cast p
        

    member this.Draw(canvas:SKCanvas) =
        canvas.DrawPoints(SKPointMode.Lines, grid_pts, paint_silver)
        canvas.DrawPoints(SKPointMode.Lines, axis_pts, paint_black)
        draw_labels canvas
        draw_models canvas
        canvas.DrawPoints(SKPointMode.Lines, tick_pts, paint_black)
        draw_legend canvas
        
