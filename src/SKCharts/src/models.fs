namespace SKCharts

open System
open System.Numerics
open SkiaSharp

[<Struct>]
type ChartType =
    | Points
    | Line
    | Surface
    | Arrows

[<Struct>]
type Bounds2 = {
    xmin: float
    xmax: float
    ymin: float
    ymax: float
}

[<Struct>]
type Bounds3 = {
    xmin: float
    xmax: float
    ymin: float
    ymax: float
    zmin: float
    zmax: float
}

type Model2 = {
    mutable name: string
    kind:     ChartType
    color:    SKColor    
    xvalues:  array<float>
    yvalues:  array<float>
    vertices: array<Vector2>
    points:   array<SKPoint>
}

type Model3 = {
    name: string
    kind: ChartType
    color: SKColor
    xvalues: array<float>
    yvalues: array<float>
    zvalues: array<float>
    vertices: array<Vector3>
    points:   array<SKPoint>
}

module Bounds2 =

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


    let compare (a:Bounds2) (b:Bounds2) =
        let xmin = min a.xmin b.xmin
        let xmax = max a.xmax b.xmax
        let ymin = min a.ymin b.ymin
        let ymax = max a.ymax b.ymax
        {xmin = xmin; ymin = ymin; xmax = xmax; ymax = ymax}

        
module Bounds3 =
    
    let ofArray (x:array<float>) (y:array<float>) (z:array<float>)=
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


    let compare (a:Bounds3) (b:Bounds3) =
        let xmin = min a.xmin b.xmin
        let xmax = max a.xmax b.xmax
        let ymin = min a.ymin b.ymin
        let ymax = max a.ymax b.ymax
        let zmin = min a.zmin b.zmin
        let zmax = max a.zmax b.zmax
        {xmin = xmin; ymin = ymin; xmax = xmax; ymax = ymax; zmin = zmin; zmax = zmax}


module Model2 =

    let private normalize_from_points (v:array<Vector2>) (x:array<float>) (y:array<float>) (b:Bounds2) =
        for i in 0..v.Length - 1 do
            let x' = (x[i] - b.xmin) / (b.xmax - b.xmin)
            let y' = (y[i] - b.ymin) / (b.ymax - b.ymin)
            v[i] <- Vector2(float32 x', float32 y')
    
    let private normalize_from_line (v:array<Vector2>) (x:array<float>) (y:array<float>) (b:Bounds2) =
        let n = v.Length
        let N = (2 * n - 2)

        let x0 = (x[0] - b.xmin) / (b.xmax - b.xmin)
        let y0 = (y[0] - b.ymin) / (b.ymax - b.ymin)
        v[0] <- Vector2(float32 x0, float32 y0)

        let mutable i = 1
        let mutable j = 1
        while i + 1 < v.Length && j + 1 < x.Length do
            let x' = (x[j] - b.xmin) / (b.xmax - b.xmin)
            let y' = (y[j] - b.ymin) / (b.ymax - b.ymin)
            v[i + 0] <- Vector2(float32 x', float32 y')
            v[i + 1] <- Vector2(float32 x', float32 y')
            i <- i + 2
            j <- j + 1

        let xn = (x[x.Length - 1] - b.xmin) / (b.xmax - b.xmin)
        let yn = (y[y.Length - 1] - b.ymin) / (b.ymax - b.ymin)
        v[v.Length - 1] <- Vector2(float32 x0, float32 y0)

        
        
    let create (kind:ChartType) (pts:array<float * float>) (color:SKColor) :Model2 =
        match kind with
        | Points ->
            let v = Array.zeroCreate<Vector2> (pts.Length)
            let p = Array.zeroCreate<SKPoint> (pts.Length)
            let (x,y) = Array.unzip pts
            let b = Bounds2.ofArray x y 
            normalize_from_points v x y b
            {name = ""; xvalues = x; yvalues = y; points = p; vertices = v; color = color; kind = Points}
        | Line ->
            let v = Array.zeroCreate<Vector2> (2 * pts.Length - 2)
            let p = Array.zeroCreate<SKPoint> (2 * pts.Length - 2)
            let (x,y) = Array.unzip pts
            let b = Bounds2.ofArray x y
            normalize_from_line v x y b
            {name = ""; xvalues = x; yvalues = y; points = p; vertices = v; color = color; kind = Line}
        | _ -> failwith "not implemented yet" 

    let createEmpty (kind:ChartType) (capacity:int) (color:SKColor) :Model2 =
        match kind with
        | Points ->
            let x = Array.zeroCreate<float> (capacity)
            let y = Array.zeroCreate<float> (capacity)
            let v = Array.zeroCreate<Vector2> (capacity)
            let p = Array.zeroCreate<SKPoint> (capacity)
            {name = ""; xvalues = x; yvalues = y; points = p; vertices = v; color = color; kind = Points}
        | Line -> 
            let x = Array.zeroCreate<float> (capacity)
            let y = Array.zeroCreate<float> (capacity)
            let v = Array.zeroCreate<Vector2> (2 * capacity - 2)
            let p = Array.zeroCreate<SKPoint> (2 * capacity - 2)
            {name = ""; xvalues = x; yvalues = y; points = p; vertices = v; color = color; kind = Line}
        | _ -> failwith "not implemented yet" 


    /// normalize x and y values into [0,1] vertices
    let normalize (bounds:Bounds2) (model:Model2) :unit =
        match model.kind with
        | Points ->
            normalize_from_points model.vertices model.xvalues model.yvalues bounds
        | Line ->
            normalize_from_line model.vertices model.xvalues model.yvalues bounds
        | _ -> failwith "Model2 cannot be of that ChartType"


    /// scales SKPoints to screen-coordinates
    let transformPts (transform:Matrix3x2) w h (model:Model2) :unit = 
        let p = model.points
        let v = model.vertices
        for i in 0..p.Length - 1 do
            let tv = Vector2.Transform(v[i], transform)
            let px = w * tv.X
            let py = h * (1f - tv.Y) 
            p[i] <- SKPoint(px, py)
        

module Model3 =

    let create (kind:ChartType) = ()


    let createEmpty (kind:ChartType) (capacity:int) = ()
