namespace SKCharts

open System
open System.Numerics
open SkiaSharp

type ChartType =
    | Points = 0
    | Line = 1
    | Surface = 2
    | Arrows = 3


module Model2 =

    [<Struct>]
    type Bounds = {
        xmin: float
        xmax: float
        ymin: float
        ymax: float
    }

    type Model = {
        mutable name: string
        kind:     ChartType
        color:    SKColor    
        strokeWidth: float32
        xvalues:  array<float>
        yvalues:  array<float>
        vertices: array<Vector2>
        points:   array<SKPoint>
    }

    module Bounds = 
        let Default = {xmin = 0.; xmax = 1.; ymin = 0.; ymax = 1.}

        let ofModel (m:Model) =
            let mutable xmin = m.xvalues[0]
            let mutable xmax = m.xvalues[0]
            let mutable ymin = m.yvalues[0]
            let mutable ymax = m.yvalues[0]
            
            for xn in m.xvalues do
                xmin <- if xmin > xn then xn else xmin
                xmax <- if xmax < xn then xn else xmax
            for yn in m.yvalues do
                ymin <- if ymin > yn then yn else ymin
                ymax <- if ymax < yn then yn else ymax

            // edge cases
            if xmin = xmax then 
                xmin <- xmin - 0.1
                xmax <- xmax + 0.1
            if ymin = ymax then
                ymin <- ymin - 0.1
                ymax <- ymax + 0.1            
            {xmin = xmin; xmax = xmax; ymin = ymin; ymax = ymax}
            
    
        let ofArray (x:array<float>) (y:array<float>) =
            let mutable xmin = x[0]
            let mutable xmax = x[0]
            let mutable ymin = y[0]
            let mutable ymax = y[0]
        
            for xn in x do
                xmin <- if xmin > xn then xn else xmin
                xmax <- if xmax < xn then xn else xmax
            for yn in y do
                ymin <- if ymin > yn then yn else ymin
                ymax <- if ymax < yn then yn else ymax

            // edge cases
            if xmin = xmax then 
                xmin <- xmin - 0.1
                xmax <- xmax + 0.1
            if ymin = ymax then
                ymin <- ymin - 0.1
                ymax <- ymax + 0.1            
            {xmin = xmin; xmax = xmax; ymin = ymin; ymax = ymax}
        

        let ofVec (vertices:array<Vector2>) =
            let mutable xmin = vertices[0].X
            let mutable xmax = vertices[0].X
            let mutable ymin = vertices[0].Y
            let mutable ymax = vertices[0].Y
        
            for v in vertices do
                xmin <- if xmin > v.X then v.X else xmin
                xmax <- if xmax < v.X then v.X else xmax
                ymin <- if ymin > v.Y then v.Y else ymin
                ymax <- if ymax < v.Y then v.Y else ymax
            {xmin = float xmin; xmax = float xmax; ymin = float ymin; ymax = float ymax}


        let compare (a:Bounds) (b:Bounds) =
            let xmin = min a.xmin b.xmin
            let xmax = max a.xmax b.xmax
            let ymin = min a.ymin b.ymin
            let ymax = max a.ymax b.ymax
            {xmin = xmin; ymin = ymin; xmax = xmax; ymax = ymax}

        /// returns true if one of Bounds value is +infinity or -infinity
        let isInf (b:Bounds) =
            let xmin_inf = b.xmin = +infinity || b.xmin = -infinity 
            let xmax_inf = b.xmax = +infinity || b.xmax = -infinity 
            let ymin_inf = b.ymin = +infinity || b.ymin = -infinity 
            let ymax_inf = b.ymax = +infinity || b.ymax = -infinity 
            xmin_inf || xmax_inf || ymin_inf || ymax_inf

        /// returns true if one of Bounds value is nan
        let isNaN (b:Bounds) =
            let xmin_nan = b.xmin = nan
            let xmax_nan = b.xmax = nan
            let ymin_nan = b.ymin = nan
            let ymax_nan = b.ymax = nan
            xmin_nan || xmax_nan || ymin_nan || ymax_nan
        
        
    let create (kind:ChartType) (x:array<float>) (y:array<float>) (color:SKColor) s :Model =
        let b = Bounds.ofArray x y 
        let v = Array.zeroCreate<Vector2> (x.Length)

        match kind with
        | ChartType.Points ->
            let p = Array.zeroCreate<SKPoint> (x.Length)
            {name = ""; xvalues = x; yvalues = y; points = p; vertices = v; color = color; kind = ChartType.Points; strokeWidth = s}
        | ChartType.Line ->
            let p = Array.zeroCreate<SKPoint> (2 * x.Length - 2)
            {name = ""; xvalues = x; yvalues = y; points = p; vertices = v; color = color; kind = ChartType.Line; strokeWidth = s}
        | _ -> failwith "not implemented yet" 
        

    let createEmpty (kind:ChartType) (capacity:int) (color:SKColor) s :Model =
        let x = Array.zeroCreate<float> (capacity)
        let y = Array.zeroCreate<float> (capacity)
        let v = Array.zeroCreate<Vector2> (capacity)

        match kind with
        | ChartType.Points ->
            let p = Array.zeroCreate<SKPoint> (capacity)
            {name = ""; xvalues = x; yvalues = y; points = p; vertices = v; color = color; kind = ChartType.Points; strokeWidth = s}
        | ChartType.Line -> 
            let p = Array.zeroCreate<SKPoint> (2 * capacity - 2)
            {name = ""; xvalues = x; yvalues = y; points = p; vertices = v; color = color; kind = ChartType.Line; strokeWidth = s}
        | _ -> failwith "not implemented yet" 


    /// normalize model-vertices to [0,1]
    let normalize (b:Bounds) (m:Model) =
        let x = m.xvalues
        let y = m.yvalues
        let v = m.vertices

        for i in 0..v.Length - 1 do
            let xn = (x[i] - b.xmin) / (b.xmax - b.xmin)
            let yn = (y[i] - b.ymin) / (b.ymax - b.ymin)
            v[i] <- Vector2(float32 xn, float32 yn)
        

module Model3 =

    [<Struct>]
    type Bounds = {
        xmin: float
        xmax: float
        ymin: float
        ymax: float
        zmin: float
        zmax: float
    }

    type Camera(elevation:float32, azimuth:float32) =
        let mutable elevation = elevation
        let mutable azimuth = azimuth
        let mutable needs_resync = true

        let azimuth_elevation (e:float32) (a:float32) =
            let e = Math.Clamp(e, -90f, 90f) * MathF.PI / 180f
            let a = Math.Clamp(a, -180f, 180f) * MathF.PI / 180f            
            let sne = MathF.Sin e
            let cne = MathF.Cos e
            let sna = MathF.Sin a
            let cna = MathF.Cos a
            Matrix4x4(
                cna, -sne * sna, cne * sna, 0f,
                sna, sne * cna, -cne * cna, 0f,
                0f, cne, sne, 0f,
                0f, 0f, 0f, 1f        
            )

        member x.Elevation
            with get() = elevation
            and set(value) = 
                elevation <- Math.Clamp(value, -90f, 90f)
                needs_resync <- true

        member x.Azimuth
            with get() = azimuth
            and set(value) =
                azimuth <- Math.Clamp(value, -180f, 180f)
                needs_resync <- true

        member x.View
            with get() =
                let t0 = Matrix4x4.CreateTranslation(-0.5f, -0.5f, -0.5f)
                let s0 = Matrix4x4.CreateScale(0.5f, 0.5f, 0.5f)
                let ae = azimuth_elevation elevation azimuth
                let t1 = Matrix4x4.CreateTranslation(0.5f, 0.5f, 0.5f)
                t0 * s0 * ae * t1
                // t0 * s0 * t1

        member x.Resync
            with get() = needs_resync
            and set(value) = needs_resync <- value
       

    type Model = {
        mutable name: string
        kind:    ChartType
        xvalues: array<float>
        yvalues: array<float>
        zvalues: array<float>
        vertices: array<Vector3>
        w: int
        h: int
        points:  array<SKPoint>
        indices: array<uint16>
        colors: array<SKColor>
        strokeWidth: float32
    }

    module Bounds = 
        let Default = {xmin = 0.; xmax = 1.; ymin = 0.; ymax = 1.; zmin = 0.; zmax = 1.}

        let ofModel (m:Model) =
            let mutable xmin = m.xvalues[0]
            let mutable xmax = m.xvalues[0]
            let mutable ymin = m.yvalues[0]
            let mutable ymax = m.yvalues[0]
            let mutable zmin = m.zvalues[0]
            let mutable zmax = m.zvalues[0]
        
            for xn in m.xvalues do
                xmin <- if xmin > xn then xn else xmin
                xmax <- if xmax < xn then xn else xmax
            for yn in m.yvalues do
                ymin <- if ymin > yn then yn else ymin
                ymax <- if ymax < yn then yn else ymax
            for zn in m.zvalues do
                zmin <- if zmin > zn then zn else zmin
                zmax <- if zmax < zn then zn else zmax 

            // edge cases
            if xmin = xmax then 
                xmin <- xmin - 0.1
                xmax <- xmax + 0.1
            if ymin = ymax then
                ymin <- ymin - 0.1
                ymax <- ymax + 0.1            
            if zmin = zmax then
                zmin <- zmin - 0.1
                zmax <- zmax + 0.1
            {xmin = xmin; xmax = xmax; ymin = ymin; ymax = ymax; zmin = zmin; zmax = zmax}


        let ofArray (x:array<float>) (y:array<float>) (z:array<float>) =
            let mutable xmin = x[0]
            let mutable xmax = x[0]
            let mutable ymin = y[0]
            let mutable ymax = y[0]
            let mutable zmin = z[0]
            let mutable zmax = z[0]
        
            for xn in x do
                xmin <- if xmin > xn then xn else xmin
                xmax <- if xmax < xn then xn else xmax
            for yn in y do
                ymin <- if ymin > yn then yn else ymin
                ymax <- if ymax < yn then yn else ymax
            for zn in z do
                zmin <- if zmin > zn then zn else zmin
                zmax <- if zmax < zn then zn else zmax 

            // edge cases
            if xmin = xmax then 
                xmin <- xmin - 0.1
                xmax <- xmax + 0.1
            if ymin = ymax then
                ymin <- ymin - 0.1
                ymax <- ymax + 0.1            
            if zmin = zmax then
                zmin <- zmin - 0.1
                zmax <- zmax + 0.1
            {xmin = xmin; xmax = xmax; ymin = ymin; ymax = ymax; zmin = zmin; zmax = zmax}
        

        let ofVec (vertices:array<Vector3>) =
            let mutable xmin = vertices[0].X
            let mutable xmax = vertices[0].X
            let mutable ymin = vertices[0].Y
            let mutable ymax = vertices[0].Y
            let mutable zmin = vertices[0].Z
            let mutable zmax = vertices[0].Z
        
            for v in vertices do
                xmin <- if xmin > v.X then v.X else xmin
                xmax <- if xmax < v.X then v.X else xmax
                ymin <- if ymin > v.Y then v.Y else ymin
                ymax <- if ymax < v.Y then v.Y else ymax
                zmin <- if zmin > v.Z then v.Z else zmin
                zmax <- if zmax < v.Z then v.Z else zmax
            {xmin = float xmin; xmax = float xmax; ymin = float ymin; ymax = float ymax; zmin = float zmin; zmax = float zmax}


        let compare (a:Bounds) (b:Bounds) =
            let xmin = min a.xmin b.xmin
            let xmax = max a.xmax b.xmax
            let ymin = min a.ymin b.ymin
            let ymax = max a.ymax b.ymax
            let zmin = min a.zmin b.zmin
            let zmax = max a.zmax b.zmax
            {xmin = xmin; ymin = ymin; xmax = xmax; ymax = ymax; zmin = zmin; zmax = zmax}
            
        /// returns true if one of Bounds value is +infinity or -infinity
        let isInf (b:Bounds) =
            let xmin_inf = b.xmin = +infinity || b.xmin = -infinity 
            let xmax_inf = b.xmax = +infinity || b.xmax = -infinity 
            let ymin_inf = b.ymin = +infinity || b.ymin = -infinity 
            let ymax_inf = b.ymax = +infinity || b.ymax = -infinity 
            let zmin_inf = b.zmin = +infinity || b.zmin = -infinity 
            let zmax_inf = b.zmax = +infinity || b.zmax = -infinity 
            xmin_inf || xmax_inf || ymin_inf || ymax_inf || zmin_inf || zmax_inf

        /// returns true if one of Bounds value is nan
        let isNaN (b:Bounds) =
            let xmin_nan = b.xmin = nan
            let xmax_nan = b.xmax = nan
            let ymin_nan = b.ymin = nan
            let ymax_nan = b.ymax = nan
            let zmin_nan = b.zmin = nan
            let zmax_nan = b.zmax = nan
            xmin_nan || xmax_nan || ymin_nan || ymax_nan || zmin_nan || zmax_nan
    

    let create (kind:ChartType) (x:array<float>) (y:array<float>) (z:array<float>) w h (color:SKColor) s = 
        let b = Bounds.ofArray x y z
        let v = Array.zeroCreate<Vector3> (x.Length)
        match kind with 
        | ChartType.Points -> 
            let p = Array.zeroCreate<SKPoint> ((w - 1) * (h - 1) * 4)
            let c = [|color|]
            let i = [||]
            c[0] <- color
            {name = ""; xvalues = x; yvalues = y; zvalues = z; vertices = v; w = w; h = h; points = p; indices = i; colors = c; kind = ChartType.Points; strokeWidth = s}
        | ChartType.Line ->
            let p = Array.zeroCreate<SKPoint> (2 * x.Length - 2)
            let c = [|color|]
            let i = [||]
            let _w = (2 * x.Length - 2)
            let _h = 1
            {name = ""; xvalues = x; yvalues = y; zvalues = z; vertices = v; w = _w; h = _h; points = p; indices = i; colors = c; kind = ChartType.Line; strokeWidth = s}
        | ChartType.Surface ->
            let p = Array.zeroCreate<SKPoint> ((w - 1) * (h - 1) * 4)
            let c = Array.zeroCreate<SKColor> ((w - 1) * (h - 1) * 4)
            let i = Array.zeroCreate<uint16> ((w - 1) * (h - 1) * 6)
            {name = ""; xvalues = x; yvalues = y; zvalues = z; vertices = v; w = w; h = h; points = p; indices = i; colors = c; kind = ChartType.Surface; strokeWidth = s}
        | _ -> failwith "this ChartType is not implemented for SKChart3"
            
            
            
    let createEmpty (kind:ChartType) w h (color:SKColor) s = 
        let x = Array.zeroCreate<float> (w * h)
        let y = Array.zeroCreate<float> (w * h)
        let z = Array.zeroCreate<float> (w * h)
        let v = Array.zeroCreate<Vector3> (w * h)
        match kind with 
        | ChartType.Points -> 
            let p = Array.zeroCreate<SKPoint> ((w - 1) * (h - 1) * 4)
            let c = [|color|]
            let i = [||]
            {name = ""; xvalues = x; yvalues = y; zvalues = z; vertices = v; w = w; h = h; points = p; indices = i; colors = c; kind = ChartType.Points; strokeWidth = s}
        | ChartType.Line ->
            let p = Array.zeroCreate<SKPoint> (2 * x.Length - 2)
            let c = [|color|]
            let i = [||]
            let _w = (2 * x.Length - 2)
            let _h = 1
            {name = ""; xvalues = x; yvalues = y; zvalues = z; vertices = v; w = _w; h = _h; points = p; indices = i; colors = c; kind = ChartType.Line; strokeWidth = s}
        | ChartType.Surface ->
            let p = Array.zeroCreate<SKPoint> ((w - 1) * (h - 1) * 4)
            let c = Array.zeroCreate<SKColor> ((w - 1) * (h - 1) * 4)
            let i = Array.zeroCreate<uint16> ((w - 1) * (h - 1) * 6)
            {name = ""; xvalues = x; yvalues = y; zvalues = z; vertices = v; w= w; h = h; points = p; indices = i; colors = c; kind = ChartType.Surface; strokeWidth = s}
        | _ -> failwith "this ChartType is not implemented for SKChart3"
            

    /// normalize model-vertices to [0,1]
    let normalize (b:Bounds) (m:Model) =
        let x = m.xvalues
        let y = m.yvalues
        let z = m.zvalues
        let v = m.vertices

        for i in 0..v.Length - 1 do
            let xn = (x[i] - b.xmin) / (b.xmax - b.xmin)
            let yn = (y[i] - b.ymin) / (b.ymax - b.ymin)
            let zn = (z[i] - b.zmin) / (b.zmax - b.zmin)
            v[i] <- Vector3(float32 xn, float32 yn, float32 zn)
    
// type SKModel =
//     | Model2 of Model2.Model
//     | Model3 of Model3.Model

