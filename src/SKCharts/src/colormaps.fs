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




type Colorbar(colormap:Colormap) =
    let colormap = match colormap with
                    | Colormap.Spring -> Colormaps.spring ()
                    | Colormap.Summer -> Colormaps.summer ()
                    | Colormap.Autumn -> Colormaps.autumn ()
                    | Colormap.Winter -> Colormaps.winter ()
                    | Colormap.Gray   -> Colormaps.gray ()
                    | _ -> failwith "not implemented yet"

    let vertices = Array.zeroCreate<SKPoint> (64 * 4)
    let colors   = Array.zeroCreate<SKColor> (64 * 4)
    let indices  = Array.zeroCreate<uint16> (64 * 4)

    let label_pts = Array.zeroCreate<SKPoint> 64
    let label_vals = Array.zeroCreate<float> 64
    let mutable label_pts_slice = Memory<SKPoint>(label_pts)

    let mutable w = 800f
    let mutable h = 600f
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

    member x.Update () =
        let transform = Matrix3x2.CreateTranslation(0.7f, 0.0f) * Matrix3x2.CreateScale(0.5f, 0.5f)
        let pos = Vector2.Transform(Vector2(1.0f, 0.5f), transform)
        let x = pos.X
        let dx = 0.01f
        let mutable y = pos.Y
        let mutable dy = (1f / 64f)
        let mutable value = 0f

        let mutable n = 0
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
            
            value <- value + dy
            y <- y + (dy / 2f)
            m <- m + 4
            i <- i + 6
            c <- c + 4
            n <- n + 1
        label_pts_slice <- Memory<SKPoint>(label_pts, 0, n)
        

    member this.Draw(canvas:SKCanvas) =
        canvas.DrawVertices(SKVertexMode.Triangles, vertices, null, colors, indices, paint)
        let slice = label_pts_slice.Span
        for i in 0..4..slice.Length - 1 do
            canvas.DrawText(label_vals[i].ToString("N3"), slice[i].X, slice[i].Y, paint)
