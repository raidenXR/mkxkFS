namespace SKCharts

open System
open System.Numerics
open SkiaSharp


type Colormap =
    | Spring = 0
    | Summer = 1
    | Autumn = 2
    | Winter = 3
    | Gray = 4
    | Hot = 5
    | Cool = 6
    | Jet = 7



module Colormaps =

    let [<Literal>] MAP_SIZE = 64
    let [<Literal>] ALPHA = 255uy

    let spring () =
        let buffer = Array.zeroCreate<SKColor> MAP_SIZE
        for i in 0..MAP_SIZE - 1 do
            let lerp = (float i) / (float MAP_SIZE)
            let r = 255uy
            let g = byte (255. * lerp)
            let b = byte (255uy - g)
            buffer[i] <- SKColor(r,g,b)
        buffer

    let summer () =
        let buffer = Array.zeroCreate<SKColor> MAP_SIZE
        for i in 0..MAP_SIZE - 1 do
            let lerp = (float i) / (float MAP_SIZE)
            let r = byte (255. * lerp)
            let g = byte (255. * 0.5 * (1. + lerp))
            let b = byte (255. - 0.4)
            buffer[i] <- SKColor(r,g,b)
        buffer

    let autumn () =
        let buffer = Array.zeroCreate<SKColor> MAP_SIZE
        for i in 0..MAP_SIZE - 1 do
            let lerp = (float i) / (float MAP_SIZE)
            let r = 255uy
            let g = byte (255. * lerp)
            let b = 0uy
            buffer[i] <- SKColor(r,g,b)
        buffer
        
    let winter () =
        let buffer = Array.zeroCreate<SKColor> MAP_SIZE
        for i in 0..MAP_SIZE - 1 do
            let lerp = (float i) / (float MAP_SIZE)
            let r = 0uy
            let g = byte (255. * lerp)
            let b = byte (255. * (1. - 0.5 + lerp))
            buffer[i] <- SKColor(r,g,b)
        buffer

    let gray () =
        let buffer = Array.zeroCreate<SKColor> MAP_SIZE
        for i in 0..MAP_SIZE - 1 do
            let lerp = (float i) / (float MAP_SIZE)
            let r = byte (255. * lerp)
            let g = byte (255. * lerp)
            let b = byte (255. * lerp)
            buffer[i] <- SKColor(r,g,b)
        buffer

    let hot () =
        let buffer = Array.zeroCreate<SKColor> MAP_SIZE
        for n in 0..MAP_SIZE - 1 do
            let lerp = (float n) / (float MAP_SIZE)
            let n1 = int (3.0 * float MAP_SIZE / 8.0)
            let i = int ((float MAP_SIZE - 1.0) * lerp)

            let r = if i < n1 then (1.0 * (float i + 1.0) / float n1) else 1.0
            let g = if i < n1 then 0.0 else (if (i >= n1 && i < 2 * n1) then (1.0 * (float i + 1. - float n1) / float n1) else 1.0)
            let b = if i < 2 * n1 then 0.0 else (1.0 * (float i + 1. - 2. * float n1) / (float MAP_SIZE - 2.0 * float n1))
            buffer[n] <- SKColor(byte (r * 255.), byte (g * 255.), byte (b * 255.))
        buffer

    let cool () =
        let buffer = Array.zeroCreate<SKColor> MAP_SIZE
        for n in 0..MAP_SIZE - 1 do
            let lerp = (float n) / (float MAP_SIZE)
            let i = int (float (MAP_SIZE - 1) * lerp)
            let _array = 1.0 * (float i) / (float MAP_SIZE - 1.0)

            let r = byte (255. * _array)
            let g = byte (255. * (1. - _array))
            let b = 255uy
            buffer[n] <- SKColor(r, g, b)
        buffer

    let jet () =
        let buffer = Array.zeroCreate<SKColor> MAP_SIZE
        let n = int (Math.Ceiling(float MAP_SIZE / 4.0))
        let cMatrix = Array2D.zeroCreate<float> MAP_SIZE 3
        let mutable nMod = 0
        let array1 = Array.zeroCreate<float> (3 * n - 1)
        let red   = Array.zeroCreate<int> array1.Length
        let green = Array.zeroCreate<int> array1.Length
        let blue  = Array.zeroCreate<int> array1.Length

        for i in 0..array1.Length - 1 do
            if i < n then 
                array1[i] <- float (i + 1) / float n
            elif i >= n && i < 2 * n - 1 then 
                array1[i] <- 1.0
            elif i >= 2 * n - 1 then 
                array1[i] <- 3.0 * float (n - 1 - i) / (float n)
            green[i] <- int (Math.Ceiling(float n / 2.0)) - nMod + i
            red[i]   <- green[i] + n
            blue[i]  <- green[i] - n

        let mutable nb = 0
        for i in 0..blue.Length - 1 do
            if blue[i] > 0 then nb <- nb + 1

        for i in 0..MAP_SIZE - 1 do
            for j in 0..red.Length - 1 do 
                if i = red[j] && red[j] < MAP_SIZE then cMatrix[i,0] <- array1[i - red[0]]
            for j in 0..green.Length - 1 do 
                if i = green[j] && green[j] < MAP_SIZE then cMatrix[i,1] <- array1[i - green[0]]
            for j in 0..blue.Length - 1 do 
                if i = blue[j] && blue[j] >= 0 then cMatrix[i,2] <- array1[array1.Length - 1 - nb + i]

        for i in 0..MAP_SIZE - 1 do
            let r = byte (cMatrix[i,0] * 255.)
            let g = byte (cMatrix[i,1] * 255.)
            let b = byte (cMatrix[i,2] * 255.)
            buffer[i] <- SKColor(r,g,b)
        buffer
            



type Colorbar(colormap:Colormap) =
    let colormap = match colormap with
                    | Colormap.Spring -> Colormaps.spring ()
                    | Colormap.Summer -> Colormaps.summer ()
                    | Colormap.Autumn -> Colormaps.autumn ()
                    | Colormap.Winter -> Colormaps.winter ()
                    | Colormap.Gray   -> Colormaps.gray ()
                    | Colormap.Hot    -> Colormaps.hot ()
                    | Colormap.Cool   -> Colormaps.cool ()
                    | Colormap.Jet    -> Colormaps.jet ()
                    | _ -> failwith "not implemented yet"

    let vertices = Array.zeroCreate<SKPoint> (Colormaps.MAP_SIZE * 4)
    let colors   = Array.zeroCreate<SKColor> (Colormaps.MAP_SIZE * 4)
    let indices  = Array.zeroCreate<uint16> (Colormaps.MAP_SIZE * 6)

    let tick_pts  = Array.zeroCreate<SKPoint> (Colormaps.MAP_SIZE * 2)
    let label_pts = Array.zeroCreate<SKPoint> Colormaps.MAP_SIZE
    let label_vals = Array.zeroCreate<float> Colormaps.MAP_SIZE
    let mutable label_pts_slice = Memory<SKPoint>(label_pts)

    let mutable w = 800f
    let mutable h = 600f
    let mutable b: Model3.Bounds = {xmin = 0; xmax = 1; ymin = 0; ymax = 1; zmin = 0; zmax = 1}
    let mutable border = SKRect()
    let mutable is_disposed = false

    let paint = new SKPaint(
        Color = SKColors.Black,
        StrokeWidth = 2f,
        IsAntialias = true,
        TextSize = 16f
    )

    interface IDisposable with
        member this.Dispose() =
            if not is_disposed then
                paint.Dispose()
            is_disposed <- true

    member x.Bounds
        with get() = b
        and set(value) = b <- value

    member x.Colormap
        with get() = ReadOnlySpan<SKColor>(colormap)
    
    member x.W 
        with get() = w
        and set(value) = w <- value

    member x.H 
        with get() = h
        and set(value) = h <- value
    

    member x.Update () =
        let transform = Matrix3x2.CreateTranslation(0.7f, 0.0f) * Matrix3x2.CreateScale(0.5f, 0.5f)
        let pos = Vector2.Transform(Vector2(1.0f, 0.5f), transform)
        let x = pos.X
        let dx = 0.01f
        let mutable y = pos.Y
        let mutable dy = (1f / 64f)
        let mutable value = 0f

        let mutable n = 0
        let mutable j = 0
        let mutable i = 0
        let mutable c = 0
        let mutable m = 0
        while m < vertices.Length do
            vertices[m + 0] <- SKPoint(w * x, h * (1f - y))
            vertices[m + 1] <- SKPoint(w * (x + dx), h * (1f - y))
            vertices[m + 2] <- SKPoint(w * (x + dx), h * (1f - y - dy))
            vertices[m + 3] <- SKPoint(w * x, h * (1f - y - dy))

            indices[i + 0] <- uint16 (m + 0)
            indices[i + 1] <- uint16 (m + 1)
            indices[i + 2] <- uint16 (m + 3)
            indices[i + 3] <- uint16 (m + 3)
            indices[i + 4] <- uint16 (m + 1)
            indices[i + 5] <- uint16 (m + 2)

            colors[c + 0] <- colormap[int ((float32 (Colormaps.MAP_SIZE - 1)) * value)]
            colors[c + 1] <- colormap[int ((float32 (Colormaps.MAP_SIZE - 1)) * value)]
            colors[c + 2] <- colormap[int ((float32 (Colormaps.MAP_SIZE - 1)) * value)]
            colors[c + 3] <- colormap[int ((float32 (Colormaps.MAP_SIZE - 1)) * value)]

            if m % 48 = 0 then 
                label_vals[n] <- (float value) * (b.zmax - b.zmin) + b.zmin
                label_pts[n]  <- SKPoint(w * (x + dx + 0.01f), h * (1f - y))
                tick_pts[j + 0] <- SKPoint(w * (x + dx + 0.00f), h * (1f - y) - 9f)
                tick_pts[j + 1] <- SKPoint(w * (x + dx + 0.01f), h * (1f - y) - 9f)
                n <- n + 1
                j <- j + 2
            
            value <- value + dy
            y <- y + (dy / 2f)
            
            m <- m + 4
            i <- i + 6
            c <- c + 4
        label_pts_slice <- Memory<SKPoint>(label_pts, 0, n)
        

    member this.Draw(canvas:SKCanvas) =
        canvas.DrawVertices(SKVertexMode.Triangles, vertices, null, colors, indices, paint)
        canvas.DrawPoints(SKPointMode.Lines, tick_pts, paint)
        let slice = label_pts_slice.Span
        for i in 0..slice.Length - 1 do
            canvas.DrawText(label_vals[i].ToString("N3"), slice[i].X, slice[i].Y, paint)
