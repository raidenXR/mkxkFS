namespace SKCharts

open System
open System.Collections.Generic
open System.Numerics
open SkiaSharp


type ISKChart =
    abstract member Update: unit -> unit
    abstract member Draw: SKCanvas -> unit    


type SKChart2(models':list<string * Model2.Model>) =
    
    let mutable bounds = Model2.Bounds.Default

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
    let mutable xtitle = "X"
    let mutable ytitle = "Y"
    let mutable is_disposed = false

    let models = List<Model2.Model>()

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
    
    /// get bounds of the SKChart2
    let get_bounds () =
        match models.Count with
        | 0 -> Model2.Bounds.Default
        | 1 -> Model2.Bounds.ofModel models[0]
        | _ ->
            let mutable b = Model2.Bounds.ofModel models[0]
            for i in 1..models.Count - 1 do 
                b <- Model2.Bounds.compare (Model2.Bounds.ofModel models[i]) b
            b


    /// get bounds of the SKChart2, compared to cached value
    let get_bounds_cached () =
        match models.Count with
        | 0 -> Model2.Bounds.Default
        | 1 -> 
            let b0 = Model2.Bounds.ofModel models[0]
            if Model2.Bounds.isInf bounds || Model2.Bounds.isNaN bounds then b0 else Model2.Bounds.compare bounds b0
        | _ -> 
            let b0 = Model2.Bounds.ofModel models[0]
            let mutable b = if Model2.Bounds.isInf bounds || Model2.Bounds.isNaN bounds then b0 else Model2.Bounds.compare bounds b0
            for i in 1..models.Count - 1 do 
                b <- Model2.Bounds.compare (Model2.Bounds.ofModel models[i]) b
            b
        
    /// updates bounds, applies noramization and transformation
    let update_state a =
        ignore a 
        // bounds <- get_bounds ()
        for m in models do 
            Model2.normalize bounds m
            transform_pts m transform


    let add_model = models.Add >> update_state

    let remove_model =  models.Remove >> update_state

    let remove_model_at = models.RemoveAt >> update_state


    let update_grid () = 
        let mutable i = 0
        for dv in 0.2f..0.2f..1f do
            grid_pts[i + 0] <- cast (Vector2.Transform(Vector2(dv, 0f), transform))
            grid_pts[i + 1] <- cast (Vector2.Transform(Vector2(dv, 1f), transform))
            grid_pts[i + 2] <- cast (Vector2.Transform(Vector2(0f, dv), transform))
            grid_pts[i + 3] <- cast (Vector2.Transform(Vector2(1f, dv), transform))
            i  <- i + 4


    let update_axes () =
        for i in 0..axis.Length - 1 do
            axis_pts[i] <- cast (Vector2.Transform(axis[i], transform))

    let update_ticks () = 
        let mutable i = 0
        for dv in 0.2f..0.2f..1f do
            tick_pts[i + 0] <- cast (Vector2.Transform(Vector2(dv, 0f), transform))
            tick_pts[i + 1] <- cast (Vector2.Transform(Vector2(dv, -0.05f), transform))
            tick_pts[i + 2] <- cast (Vector2.Transform(Vector2(0f, dv), transform))
            tick_pts[i + 3] <- cast (Vector2.Transform(Vector2(-0.05f, dv), transform))
            i  <- i + 4
        

    let update_labels () = 
        let b = bounds
        let mutable i = 0
        for dv in 0.2f..0.2f..1f do
            label_vals[i + 0] <- (float dv) * (b.xmax - b.xmin) + b.xmin
            label_vals[i + 1] <- (float dv) * (b.ymax - b.ymin) + b.ymin
            label_pts[i + 0]  <- cast (Vector2.Transform(Vector2(dv - 0.05f, -0.1f), transform))
            label_pts[i + 1]  <- cast (Vector2.Transform(Vector2(-0.1f - 0.05f, dv), transform))
            i  <- i + 2
        label_pts_slice <- Memory<SKPoint>(label_pts, 0, i)


    /// runs update_state (), nothing more
    let update_models () =
        update_state ()
    

    let draw_labels (canvas:SKCanvas) = 
        let slice  = label_pts_slice.Span
        let len = slice.Length - 2

        for i in 0..2..len - 2 do
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
            paint_model.StrokeWidth <- m.strokeWidth
            match m.kind with
            | ChartType.Line -> 
                canvas.DrawPoints(SKPointMode.Lines, m.points, paint_model)
            | ChartType.Points ->
                canvas.DrawPoints(SKPointMode.Points, m.points, paint_model)
            | _ -> failwith "Kind not implemented, in draw_models"


    do 
        models' |> List.iter (fun (name,model) -> models.Add({model with name = name}))
        bounds <- get_bounds ()
        update_grid ()
        update_axes ()
        update_ticks ()
        update_labels ()
        update_models ()

        let t = Matrix3x2.CreateTranslation(0.6f, 0.6f) * Matrix3x2.CreateScale(0.5f, 0.5f)
        let p = Vector2.Transform(Vector2(1f, 0.8f), t)
        legend <- cast p


    interface IDisposable with 
        member this.Dispose() = 
            if not is_disposed then
                paint_black.Dispose()
                paint_silver.Dispose()
                paint_model.Dispose()
            is_disposed <- true

    interface ISKChart with
        member this.Update() =
            bounds <- get_bounds ()
            update_grid ()
            update_axes ()
            update_ticks ()
            update_labels ()
            update_models ()

            let t = Matrix3x2.CreateTranslation(0.6f, 0.6f) * Matrix3x2.CreateScale(0.5f, 0.5f)
            let p = Vector2.Transform(Vector2(1f, 0.8f), t)
            legend <- cast p

        member this.Draw(canvas:SKCanvas) =
            canvas.DrawColor(this.Background)
            canvas.DrawPoints(SKPointMode.Lines, grid_pts, paint_silver)
            canvas.DrawPoints(SKPointMode.Lines, axis_pts, paint_black)
            draw_labels canvas
            draw_models canvas
            canvas.DrawPoints(SKPointMode.Lines, tick_pts, paint_black)
            draw_legend canvas

    static member Empty = new SKChart2([])

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

    member this.Models with get() = (models :> IList<Model2.Model>)

    member val Background = SKColors.White with get, set

    member this.ResetBounds() =
        bounds <- Model2.Bounds.Default
    
    member this.UpdateModels () = 
        update_models ()        
    
    
    member this.Update() =
        bounds <- get_bounds ()
        update_grid ()
        update_axes ()
        update_ticks ()
        update_labels ()
        update_models ()

        let t = Matrix3x2.CreateTranslation(0.6f, 0.6f) * Matrix3x2.CreateScale(0.5f, 0.5f)
        let p = Vector2.Transform(Vector2(1f, 0.8f), t)
        legend <- cast p
        
    member this.UpdateCachedBounds() =
        bounds <- get_bounds_cached ()
        update_grid ()
        update_axes ()
        update_ticks ()
        update_labels ()
        update_models ()

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
        




type SKChart3(models':list<Model3.Model>, colormap:Colormap) = 

    let mutable bounds = Model3.Bounds.Default
    let colorbar = new Colorbar(colormap)
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

    let mutable is_disposed = false
    let models = System.Collections.Generic.List<Model3.Model>()

    let mutable w = 800f
    let mutable h = 600f

    let mutable xtitle = "X"
    let mutable ytitle = "Y"
    let mutable ztitle = "Z"

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
        StrokeWidth = 2.0f, 
        IsAntialias = true,
        TextSize = 16f        
    )

    let cast2d (vec:Vector3) = SKPoint(w * vec.X, h * (1f - vec.Y))

    /// transform normalized vertices to screen-coordinates as pts
    let transform_pts (m:Model3.Model) (transform:Matrix4x4) =
        match m.kind with
        | ChartType.Line ->
            let v = m.vertices
            let p = m.points
            let transform = camera.View
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
        | ChartType.Points -> 
            let width  = m.w
            let height = m.h
            let vertices = m.vertices
            let elevation = camera.Elevation
            let azimuth = camera.Azimuth
            let transform = camera.View
            // draw mesh
            let mutable v = 0
            for i = 0 to width - 2 do
                let mutable ii = i
                if elevation >= 0f then 
                    ii <- i
                    if azimuth >= -180f && azimuth < 0f then ii <- width - 2 - i
                else
                    ii <- width - 2 - i
                    if azimuth >= -180f && azimuth < 0f then ii <- i

                for j = 0 to height - 2 do
                    let mutable jj = j
                    if elevation < 0f then jj <- height - 2 - j

                    m.points[v + 0] <- cast2d (Vector3.Transform(vertices[width * ii + jj], transform))
                    m.points[v + 1] <- cast2d (Vector3.Transform(vertices[width * ii + jj + 1], transform))
                    m.points[v + 2] <- cast2d (Vector3.Transform(vertices[width * (ii + 1) + jj + 1], transform))
                    m.points[v + 3] <- cast2d (Vector3.Transform(vertices[width * (ii + 1) + jj], transform))
                    v <- v + 4
        | ChartType.Surface ->
            let width  = m.w
            let height = m.h
            let vertices = m.vertices
            let elevation = camera.Elevation
            let azimuth = camera.Azimuth
            let transform = camera.View
            let colormap = colorbar.Colormap
            // draw mesh
            let mutable v = 0
            let mutable c = 0
            let mutable l = 0

            for i = 0 to width - 2 do
                let mutable ii = i
                if elevation >= 0f then 
                    ii <- i
                    if azimuth >= -180f && azimuth < 0f then ii <- width - 2 - i
                else
                    ii <- width - 2 - i
                    if azimuth >= -180f && azimuth < 0f then ii <- i

                for j = 0 to height - 2 do
                    let mutable jj = j
                    if elevation < 0f then jj <- height - 2 - j

                    let vec0 = vertices[width * ii + jj]
                    let vec1 = vertices[width * ii + jj + 1]
                    let vec2 = vertices[width * (ii + 1) + jj + 1]
                    let vec3 = vertices[width * (ii + 1) + jj]
                    
                    m.points[v + 0] <- cast2d (Vector3.Transform(vec0, transform))
                    m.points[v + 1] <- cast2d (Vector3.Transform(vec1, transform))
                    m.points[v + 2] <- cast2d (Vector3.Transform(vec2, transform))
                    m.points[v + 3] <- cast2d (Vector3.Transform(vec3, transform))

                    m.indices[l + 0] <- uint16 (v + 0)
                    m.indices[l + 1] <- uint16 (v + 1)
                    m.indices[l + 2] <- uint16 (v + 3)
                    m.indices[l + 3] <- uint16 (v + 3)
                    m.indices[l + 4] <- uint16 (v + 1)
                    m.indices[l + 5] <- uint16 (v + 2)

                    m.colors[c + 0] <- colormap[int ((float32 (Colormaps.MAP_SIZE - 1) * vec0.Z))]
                    m.colors[c + 1] <- colormap[int ((float32 (Colormaps.MAP_SIZE - 1) * vec1.Z))]
                    m.colors[c + 2] <- colormap[int ((float32 (Colormaps.MAP_SIZE - 1) * vec2.Z))]
                    m.colors[c + 3] <- colormap[int ((float32 (Colormaps.MAP_SIZE - 1) * vec3.Z))]
            
                    v <- v + 4
                    l <- l + 6    
                    c <- c + 4
        | _ -> failwith "this not implemented"

    /// resets bounds of the SKChart3
    let get_bounds () =
        match models.Count with
        | 0 -> Model3.Bounds.Default
        | 1 ->        
            Model3.Bounds.ofModel models[0]
        | _ ->
            let mutable b = Model3.Bounds.ofModel models[0]
            for i in 1..models.Count - 1 do 
                b <- Model3.Bounds.compare (Model3.Bounds.ofModel models[i]) b
            b

    /// resets bounds of the SKChart3
    let get_bounds_cached () =
        match models.Count with
        | 0 -> Model3.Bounds.Default
        | 1 -> 
            let b0 = Model3.Bounds.ofModel models[0]
            if Model3.Bounds.isInf bounds || Model3.Bounds.isNaN bounds then b0 else Model3.Bounds.compare bounds b0            
        | _ -> 
            let b0 = Model3.Bounds.ofModel models[0]
            let mutable b = if Model3.Bounds.isInf bounds || Model3.Bounds.isNaN bounds then b0 else Model3.Bounds.compare bounds b0            
            for i in 1..models.Count - 1 do 
                b <- Model3.Bounds.compare (Model3.Bounds.ofModel models[i]) b
            b

            
    let update_state a =
        ignore a
        // bounds <- get_bounds ()
        let transform = camera.View
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


    /// runs update_state (), nothing more
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
                paint_model.StrokeWidth <- m.strokeWidth
                canvas.DrawPoints(SKPointMode.Points, m.points, paint_model)
            | ChartType.Line ->
                paint_model.Color <- m.colors[0]
                paint_model.StrokeWidth <- m.strokeWidth
                canvas.DrawPoints(SKPointMode.Lines, m.points, paint_model)
            | ChartType.Surface ->
                canvas.DrawVertices(SKVertexMode.Triangles, m.points, null, m.colors, m.indices, paint_model)
            | _ -> failwith "this ChartType is not valid for SKChart3"


    do
        models' |> List.iter (fun model -> models.Add(model))
        bounds <- get_bounds ()
        update_grid ()
        update_axis ()
        update_ticks ()
        update_labels ()
        update_models ()
        colorbar.Bounds <- bounds
        colorbar.W <- w
        colorbar.H <- h
        colorbar.Update()
        camera.Resync <- false
            

    interface IDisposable with 
        member this.Dispose() = 
            if not is_disposed then
                paint_black.Dispose()
                paint_silver.Dispose()
                paint_model.Dispose()
                (colorbar :> IDisposable).Dispose()
            is_disposed <- true

    interface ISKChart with
        member this.Update() =
            bounds <- get_bounds ()
            update_grid ()
            update_axis ()
            update_ticks ()
            update_labels ()
            update_models ()
            colorbar.Bounds <- bounds
            colorbar.W <- w
            colorbar.H <- h
            colorbar.Update()
            camera.Resync <- false

        member this.Draw(canvas:SKCanvas) =
            canvas.DrawColor(this.Background)
            canvas.DrawPoints(SKPointMode.Lines, grid_pts, paint_silver)
            canvas.DrawPoints(SKPointMode.Lines, axis_pts, paint_black)
            canvas.DrawPoints(SKPointMode.Lines, tick_pts, paint_black)
            draw_labels (canvas)
            draw_models (canvas)
            colorbar.Draw(canvas)        

    static member Empty = new SKChart3([], Colormap.Hot)
    
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

    member this.Models with get() = (models :> IList<Model3.Model>)

    member val Background = SKColors.White with get, set

    /// reset bounds to Bounds.Default
    member this.ResetBounds() =
        bounds <- Model3.Bounds.Default

    member this.UpdateModels () = 
        update_models ()        

    /// updates SKChart, while restoring bounds to current bounds
    member this.Update() =
        bounds <- get_bounds ()
        update_grid ()
        update_axis ()
        update_ticks ()
        update_labels ()
        update_models ()
        colorbar.Bounds <- bounds
        colorbar.W <- w
        colorbar.H <- h
        colorbar.Update()
        camera.Resync <- false
        
    /// updates SKChart, while comparing with cached bounds
    member this.UpdateCachedBounds() =
        bounds <- get_bounds_cached ()
        update_grid ()
        update_axis ()
        update_ticks ()
        update_labels ()
        update_models ()
        colorbar.Bounds <- bounds
        colorbar.W <- w
        colorbar.H <- h
        colorbar.Update()
        camera.Resync <- false

    member this.Draw(canvas:SKCanvas) =
        canvas.DrawPoints(SKPointMode.Lines, grid_pts, paint_silver)
        canvas.DrawPoints(SKPointMode.Lines, axis_pts, paint_black)
        canvas.DrawPoints(SKPointMode.Lines, tick_pts, paint_black)
        draw_labels (canvas)
        draw_models (canvas)
        colorbar.Draw(canvas)



/// Composition of SKChart2 and SKChart3, state - pattern(?)
[<AllowNullLiteral>]
type SKChart(c2:SKChart2, c3:SKChart3) =
    let mutable c2 = c2
    let mutable c3 = c3
    let tags = new System.Collections.Generic.List<string * int>()
    let args = new System.Collections.ArrayList()
    let mutable is_c2 = true
    let mutable is_c3 = false
    let mutable is_disposed = false

    new() = new SKChart(new SKChart2([]), new SKChart3([], Colormap.Hot))

    interface IDisposable with
        member this.Dispose() =
            if not is_disposed then
                (c2 :> IDisposable).Dispose()
                (c3 :> IDisposable).Dispose()
            is_disposed <- true
    
    member this.Tags with get() = tags

    member this.Args with get() = args

    member this.C2 with get() = c2 and set(value) = c2 <- value

    member this.C3 with get() = c3 and set(value) = c3 <- value

    member this.W with get() = if is_c2 then c2.W else c3.W

    member this.H with get() = if is_c2 then c2.H else c3.H

    member this.Bounds2 with get() = c2.Bounds

    member this.Bounds3 with get() = c3.Bounds

    member this.AsSKChart2() = 
        is_c2 <- true
        is_c3 <- false
        c2

    member this.AsSKChart3() =
        is_c3 <- true
        is_c2 <- false
        c3

    member this.IsSKChart2 with get() = is_c2

    member this.IsSKChart3 with get() = is_c3
        
    member this.Background 
        with get() =
            if this.IsSKChart2 then c2.Background else c3.Background
        and set(value) =
            if this.IsSKChart2 then c2.Background <- value else c3.Background <- value

    member this.Update() =
        if is_c2 then c2.Update()
        elif is_c3 then c3.Update()

    member this.Draw(canvas:SKCanvas) =
        if is_c2 then c2.Draw(canvas)
        elif is_c3 then c3.Draw(canvas)
