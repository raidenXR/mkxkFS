namespace SKCharts

open System
open System.Numerics
open SkiaSharp


type SKChart2(models':list<string * Model2.Model>) =
    
    let mutable bounds: Model2.Bounds = {xmin = 0.; xmax = 1.; ymin = 0.; ymax = 1.}

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

    let models = System.Collections.Generic.List<Model2.Model>()

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

    /// transform normalized vertices to screen-coordinates as pts
    let transform_pts (m:Model2.Model) (transform:Matrix3x2) = 
        let v = m.vertices
        let p = m.points        
        match m.kind with
        | ChartType.Points ->
            for i in 0..p.Length - 1 do
                p[i] <- cast (Vector2.Transform(v[i], transform))
        | ChartType.Line -> 
            p[0] <- cast (Vector2.Transform(v[0], transform))
            let mutable i = 1
            let mutable j = 1
            while i + 1 < p.Length && j + 1 < v.Length do
                let tp = cast (Vector2.Transform(v[j], transform))
                p[i + 0] <- tp
                p[i + 1] <- tp
                i <- i + 2
                j <- j + 1
            p[p.Length - 1] <- cast (Vector2.Transform(v[v.Length - 1], transform))
        | _ -> failwith "this CharType is not valid for SKChart2"
    
        
    /// updates bounds, applies noramization and transformation
    let update_state a =
        ignore a 
        bounds <- if models.Count = 1 then (Model2.Bounds.ofArray models[0].xvalues models[0].yvalues) else bounds
        for m in models do
            let mb = Model2.Bounds.ofArray m.xvalues m.yvalues
            bounds <- Model2.Bounds.compare mb bounds
        for m in models do 
            Model2.normalize bounds m
            transform_pts m transform


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

        for i in 0..2..len - 1 do
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
            | ChartType.Line -> 
                canvas.DrawPoints(SKPointMode.Lines, m.points, paint_model)
            | ChartType.Points ->
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

    member this.Bounds with get() = bounds

    member this.AddModel(model:Model2.Model) = add_model model

    member this.RemoveModel(model:Model2.Model) = remove_model model

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
        




type SKChart3(models:list<Model3.Model>, colormap:Colormap) = 
    let colorbar = new Colorbar(colormap)
    let colormap = match colormap with
                    | Colormap.Spring -> Colormaps.spring ()
                    | Colormap.Summer -> Colormaps.summer ()
                    | Colormap.Autumn -> Colormaps.autumn ()
                    | Colormap.Winter -> Colormaps.winter ()
                    | Colormap.Gray   -> Colormaps.gray ()
                    | _ -> failwith "not implemented yet"
    let camera = Model3.Camera(30f, -37.5f)
    let axis = [|
        Vector3(0f, 0f, 0f)
        Vector3(1f, 0f, 0f)
        Vector3(0f, 0f, 0f)
        Vector3(0f, 1f, 0f)
        Vector3(0f, 1f, 0f)
        Vector3(0f, 1f, 1f)
    |]
    let grid  = Array.zeroCreate<Vector3> 100
    let ticks = Array.zeroCreate<Vector3> 100
    let labels = Array.zeroCreate<Vector3> 100    
    
    let axis_pts = Array.zeroCreate<SKPoint> 6
    let grid_pts = Array.zeroCreate<SKPoint> 60
    let tick_pts = Array.zeroCreate<SKPoint> 60
    let label_pts = Array.zeroCreate<SKPoint> 60
    let label_vals = Array.zeroCreate<float> 100
    let mutable label_pts_slice = Memory<SKPoint>(label_pts)

    let mutable bounds: Model3.Bounds = {xmin = 0; xmax = 1; ymin = 0; ymax = 1; zmin = 0; zmax = 1}
    let mutable is_disposed = false
    let models = System.Collections.Generic.List<Model3.Model>()

    let mutable w = 800f
    let mutable h = 600f

    let mutable xtitle = String.Empty
    let mutable ytitle = String.Empty
    let mutable ztitle = String.Empty

    let mutable ximg: SKImage = null
    let mutable yimg: SKImage = null
    let mutable zimg: SKImage = null

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

    let cast2d (vec:Vector3) = SKPoint(w * vec.X, h * (1f - vec.Y))

    /// transform normalized vertices to screen-coordinates as pts
    let transform_pts (m:Model3.Model) (transform:Matrix4x4) =
        let v = m.vertices
        let p = m.points
        let elevation = camera.Elevation
        let azimuth = camera.Azimuth
        let transform = camera.View
        match m.kind with
        | ChartType.Line ->
            p[0] <- cast2d (Vector3.Transform(v[0], transform))
            let mutable i = 1
            let mutable j = 1
            while i + 1 < p.Length && j + 1 < v.Length do
                let tp = cast2d (Vector3.Transform(v[j], transform))
                p[i + 0] <- tp
                p[i + 1] <- tp
                i <- i + 2
                j <- j + 1
            p[p.Length - 1] <- cast2d (Vector3.Transform(v[v.Length - 1], transform))
        | ChartType.Points
        | ChartType.Surface ->
            let vertices = m.vertices
            // draw mesh
            let mutable i = 0
            let mutable v = 0
            let mutable c = 0
            let mutable l = 0
            let w = m.w
            let h = m.h

            while i < w - 1 do
                let mutable ii = i
                if elevation >= 0f then 
                    ii <- i
                    if azimuth >= -180f && azimuth < 0f then ii <- w - 2 - i
                else
                    ii <- w - 2 - i
                    if azimuth >= -180f && azimuth < 0f then ii <- i

                let mutable j = 0
                while j < h - 1 do
                    let mutable jj = l
                    if elevation < 0f then jj <- h - 2 - l

                    m.points[v + 0] <- cast2d (Vector3.Transform(vertices[w * ii + jj], transform))
                    m.points[v + 1] <- cast2d (Vector3.Transform(vertices[w * ii + jj + 1], transform))
                    m.points[v + 2] <- cast2d (Vector3.Transform(vertices[w * (ii + 1) + jj + 1], transform))
                    m.points[v + 3] <- cast2d (Vector3.Transform(vertices[w * (ii + 1) + jj], transform))

                    m.indices[l + 0] <- uint16 (v + 0)
                    m.indices[l + 1] <- uint16 (v + 1)
                    m.indices[l + 2] <- uint16 (v + 3)
                    m.indices[l + 3] <- uint16 (v + 3)
                    m.indices[l + 4] <- uint16 (v + 1)
                    m.indices[l + 5] <- uint16 (v + 2)

                    m.colors[c + 0] <- colormap[int ((float32 (Colormaps.MAP_SIZE - 1) * vertices[w * ii + jj].Z))]
                    m.colors[c + 1] <- colormap[int ((float32 (Colormaps.MAP_SIZE - 1) * vertices[w * ii + jj + 1].Z))]
                    m.colors[c + 2] <- colormap[int ((float32 (Colormaps.MAP_SIZE - 1) * vertices[w * (ii + 1) + jj + 1].Z))]
                    m.colors[c + 3] <- colormap[int ((float32 (Colormaps.MAP_SIZE - 1) * vertices[w * (ii + 1) + jj].Z))]
            
                    j <- j + 1
                    v <- v + 4
                    l <- l + 6    
                    c <- c + 4
                i <- i + 1
        | _ -> failwith "this not implemented"


    let update_state a =
        ignore a
        let transform = camera.View
        bounds <- if models.Count = 1 then (Model3.Bounds.ofArray models[0].xvalues models[0].yvalues models[0].zvalues) else bounds
        for m in models do
            let mb = Model3.Bounds.ofArray m.xvalues m.yvalues m.zvalues
            bounds <- Model3.Bounds.compare mb bounds
        for m in models do
            Model3.normalize bounds m
            transform_pts m transform
        
        

    let add_model = models.Add >> update_state

    let remove_model = models.Remove >> update_state

    let remove_model_at = models.RemoveAt >> update_state

    let update_grid () =
        let transform = camera.View
        let mutable i = 0
        let mutable dv = 0.2f        
        while i < 20 do
            grid_pts[i + 0] <- cast2d (Vector3.Transform(Vector3(dv, 0f, 0f), transform))
            grid_pts[i + 1] <- cast2d (Vector3.Transform(Vector3(dv, 1f, 0f), transform))
            grid_pts[i + 2] <- cast2d (Vector3.Transform(Vector3(0f, dv, 0f), transform))
            grid_pts[i + 3] <- cast2d (Vector3.Transform(Vector3(1f, dv, 0f), transform))
            dv <- dv + 0.2f
            i  <- i + 4

        dv <- 0.2f
        while i < 40 do
            grid_pts[i + 0] <- cast2d (Vector3.Transform(Vector3(0f, 1f, dv), transform))
            grid_pts[i + 1] <- cast2d (Vector3.Transform(Vector3(1f, 1f, dv), transform))
            grid_pts[i + 2] <- cast2d (Vector3.Transform(Vector3(1f, 0f, dv), transform))
            grid_pts[i + 3] <- cast2d (Vector3.Transform(Vector3(1f, 1f, dv), transform))
            dv <- dv + 0.2f
            i  <- i + 4
            
        dv <- 0.2f
        while i < 60 do
            grid_pts[i + 0] <- cast2d (Vector3.Transform(Vector3(dv, 1f, 0f), transform))
            grid_pts[i + 1] <- cast2d (Vector3.Transform(Vector3(dv, 1f, 1f), transform))
            grid_pts[i + 2] <- cast2d (Vector3.Transform(Vector3(1f, dv, 0f), transform))
            grid_pts[i + 3] <- cast2d (Vector3.Transform(Vector3(1f, dv, 1f), transform))
            dv <- dv + 0.2f
            i  <- i + 4


    let update_axis () =
        let transform = camera.View
        for i in 0..axis.Length - 1 do
            axis_pts[i] <- cast2d (Vector3.Transform(axis[i], transform))
            

    let update_ticks () =
        let transform = camera.View
        let mutable i = 0
        let mutable dv = 0.2f
        while dv < 1f do
            tick_pts[i + 0] <- cast2d (Vector3.Transform(Vector3(dv, 0f, 0f), transform))
            tick_pts[i + 1] <- cast2d (Vector3.Transform(Vector3(dv, -0.05f, 0f), transform))
            tick_pts[i + 2] <- cast2d (Vector3.Transform(Vector3(0f, dv, 0f), transform))
            tick_pts[i + 3] <- cast2d (Vector3.Transform(Vector3(-0.05f, dv, 0f), transform))
            tick_pts[i + 4] <- cast2d (Vector3.Transform(Vector3(0f, 1f, dv), transform))
            tick_pts[i + 5] <- cast2d (Vector3.Transform(Vector3(-0.05f, 1.05f, dv), transform))
            dv <- dv + 0.2f
            i  <- i + 6

    let update_labels () =
        let b = bounds
        let transform = camera.View
        let mutable i = 0
        let mutable dv = 0.2f
        while dv < 1f do
            label_vals[i + 0] <- (float dv) * (b.xmax - b.xmin) + b.xmin 
            label_vals[i + 1] <- (float dv) * (b.ymax - b.ymin) + b.ymin 
            label_vals[i + 2] <- (float dv) * (b.zmax - b.zmin) + b.zmin 
            label_pts[i + 0] <- cast2d (Vector3.Transform(Vector3(dv, -0.1f, 0f), transform))
            label_pts[i + 1] <- cast2d (Vector3.Transform(Vector3(-0.1f, dv, 0f), transform))
            label_pts[i + 2] <- cast2d (Vector3.Transform(Vector3(-0.1f, 1.1f, dv), transform))
            dv <- dv + 0.2f
            i  <- i + 3

        label_vals[i + 0] <- 1. * (b.xmax - b.xmin) + b.xmin 
        label_vals[i + 1] <- 1. * (b.ymax - b.ymin) + b.ymin 
        label_vals[i + 2] <- 1. * (b.zmax - b.zmin) + b.zmin 
        label_pts[i + 0] <- cast2d (Vector3.Transform(Vector3(1f, -0.1f, 0f), transform))
        label_pts[i + 1] <- cast2d (Vector3.Transform(Vector3(-0.1f, 1f, 0f), transform))
        label_pts[i + 2] <- cast2d (Vector3.Transform(Vector3(-0.1f, 1.1f, 1f), transform))
        i <- i + 3
        label_pts_slice <- Memory(label_pts, 0, i)


    let update_models () =
        update_state ()

    let draw_labels (canvas:SKCanvas) =
        let slice = label_pts_slice.Span
        let len = label_pts_slice.Length - 3
        for i in 0..3..len - 1 do
            canvas.DrawText(label_vals[i + 0].ToString("N3"), slice[i + 0].X, slice[i + 0].Y, paint_black)
            canvas.DrawText(label_vals[i + 1].ToString("N3"), slice[i + 1].X, slice[i + 1].Y, paint_black)
            canvas.DrawText(label_vals[i + 2].ToString("N3"), slice[i + 2].X, slice[i + 2].Y, paint_black)
            
        if ximg <> null then
            canvas.DrawImage(ximg, slice[len + 0].X, slice[len + 0].Y, paint_black)
        else 
            canvas.DrawText(xtitle, slice[len + 0].X, slice[len + 0].Y, paint_black)
        
        if yimg <> null then
            canvas.DrawImage(yimg, slice[len + 1].X, slice[len + 1].Y, paint_black)
        else 
            canvas.DrawText(ytitle, slice[len + 1].X, slice[len + 1].Y, paint_black)
        
        if zimg <> null then
            canvas.DrawImage(zimg, slice[len + 2].X, slice[len + 2].Y, paint_black)
        else 
            canvas.DrawText(ztitle, slice[len + 2].X, slice[len + 2].Y, paint_black)


    let draw_models (canvas:SKCanvas) =
        for m in models do
            match m.kind with
            | ChartType.Points ->
                paint_model.Color <- m.colors[0]
                canvas.DrawPoints(SKPointMode.Points, m.points, paint_model)
            | ChartType.Line ->
                paint_model.Color <- m.colors[0]
                canvas.DrawPoints(SKPointMode.Lines, m.points, paint_model)
            | ChartType.Surface ->
                canvas.DrawVertices(SKVertexMode.Triangles, m.points, null, m.colors, m.indices, paint_model)
            | _ -> failwith "this ChartType is not valid for SKChart3"
            

    interface IDisposable with 
        member this.Dispose() = 
            if not is_disposed then
                paint_black.Dispose()
                paint_silver.Dispose()
                paint_model.Dispose()
                (colorbar :> IDisposable).Dispose()
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

    member this.Bounds with get() = bounds

    member this.Camera with get() = camera

    member this.AddModel(model:Model3.Model) = add_model model

    member this.RemoveModel(model:Model3.Model) = remove_model model

    member this.RemoveModelAt(idx:int) = remove_model_at idx


    member this.Update() =
        update_grid ()
        update_axis ()
        update_ticks ()
        update_models ()
        update_labels ()
        colorbar.Bounds <- bounds
        colorbar.Update()
        camera.Resync <- false
        

    member this.Draw(canvas:SKCanvas) =
        canvas.DrawPoints(SKPointMode.Lines, grid_pts, paint_silver)
        canvas.DrawPoints(SKPointMode.Lines, axis_pts, paint_black)
        canvas.DrawPoints(SKPointMode.Lines, tick_pts, paint_black)
        draw_labels (canvas)
        draw_models (canvas)
        colorbar.Draw(canvas)
