namespace RendererFS

open System
open MKXK
open SkiaSharp
open SKCharts


type Model = 
    | RawModel2 of string * string * Model2.Model
    | RawModel3 of string * string * string * Model3.Model
    | TeXModel of string * ExprTree.Expr * Binder.BoundExpr * Colors * float32

    with member this.Name 
            with get() =
                match this with
                | RawModel2 (x,y,m) -> m.name
                | RawModel3 (x,y,z,m) -> m.name
                | TeXModel (s,f,_,_,_) -> 
                    match f with
                    | ExprTree.Assignment (name,_) -> name
                    | _ -> failwith "this Expr is not assignment"
            and set(value) = 
                match this with
                | RawModel2 (x,y,m) -> m.name <- value
                | RawModel3 (x,y,z,m) -> m.name <- value
                | TeXModel _ -> failwith "cannot set name in TeXModel"


module Models =   

    let evalModel2 (m:Maps) t f (model:Model2.Model) :unit =
        let xvalues = model.xvalues
        let yvalues = model.yvalues
        let A = m.variables[t].A
        let B = m.variables[t].B
        let dx = (B - A) / (float xvalues.Length)
        let mutable x = 0.
        
        for i in 0..xvalues.Length - 1 do
            m.variables[t].V <- ValueSome x
            xvalues[i] <- x
            yvalues[i] <- Evaluation.eval m t f    
            x <- x + dx

    let createRawModel2 tx ty x y (c:Colors) s :Model =
        let m = Model2.create ChartType.Points x y (SKColor(uint c)) s
        RawModel2 (tx, ty, m)
        

    let createTeXModel (maps:Maps) tex t (c:Colors) s :Model =
        let s': Symbols = {
            constants = maps.constants.Keys
            variables = maps.variables.Keys
            functions = maps.functions.Keys
        }
        let f = Parser.parse s' tex
        let b = Binder.bind f
        let x = Array.zeroCreate<float> 100
        let y = Array.zeroCreate<float> 100
        let m = Model2.create ChartType.Line x y (SKColor(uint c)) s
        match f with
        | ExprTree.Assignment (fname,_) -> m.name <- fname
        | _ -> ()
        evalModel2 maps t b m
        TeXModel (tex, f, b, c, s)


    let setNames (names:list<string>) (models:list<Model>) = 
        for name, model in List.zip names models do
            model.Name <- name


    let createRawModel3 tx ty tz x y z w h (c:Colors) s :Model =
        let m = Model3.create ChartType.Surface x y z w h (SKColor(uint c)) s
        RawModel3 (tx, ty, tz, m) 

        
    let evalModel3 (m:Maps) tx ty f (model:Model3.Model) :unit =
        let xvalues = model.xvalues
        let yvalues = model.yvalues
        let zvalues = model.zvalues
        let Ax = m.variables[tx].A
        let Bx = m.variables[tx].B
        let Ay = m.variables[ty].A
        let By = m.variables[ty].B
        let w = model.w
        let h = model.h
        let dx = (Bx - Ax) / (float w)
        let dy = (By - Ay) / (float h)

        for i = 0 to w - 1 do
            for j = 0 to h - 1 do
                let x = Ax + dx * (float i)
                let y = Ay + dy * (float j)
                m.variables[tx].V <- ValueSome x
                m.variables[ty].V <- ValueSome y
                xvalues[i * w + j] <- x
                yvalues[i * w + j] <- y
                zvalues[i * w + j] <- Evaluation.eval m tx f                
      

