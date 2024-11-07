namespace RendererFS

open Avalonia
open Avalonia.Controls.Primitives
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout

open System
open MKXK
open SkiaSharp
open SKCharts
open SKCharts.Avalonia


module Converter =
    open NotationFS
    open System.Linq

    let ms = new System.IO.MemoryStream(8 * 1024)

    let convert (tex:string) = 
        ms.Position <- 0
        let parser = Parser(tex)
        let exprs = parser.exprs()
        Typesetting.render exprs ms               
        ms.Position <- 0
        new Avalonia.Media.Imaging.Bitmap(ms)


    let image (tex:string) =
        ms.Position <- 0
        let parser = Parser(tex)
        let exprs = parser.exprs()
        Typesetting.render exprs ms
        ms.Position <- 0
        SKImage.FromEncodedData(ms)


module Model2 =
    
    type Model = 
        | TeXModel of string * ExprTree.Expr * Binder.BoundExpr * SKCharts.Model2D
        | Model2D of SKCharts.Model2D

    
    let createpoints x y (c:Colors) s =
        let m = SKCharts.Model2D.CreatePoints(x, y, SKColor(uint c))
        m.Paint.StrokeWidth <- s
        Model2D m

    let createline x y (c:Colors) s =
        let m = SKCharts.Model2D.CreateLine(x, y, SKColor(uint c))
        m.Paint.StrokeWidth <- s
        Model2D m

    // let evalModel (cons:Map<string,float>) (vars:Map<string,Variable>) (fns:Map<string,Binder.BoundExpr>) t f (m:SKCharts.Model2D) =
    let evalModel (m:Maps) t f (model:SKCharts.Model2D) :unit =
        let xvalues = model.Xvalues
        let yvalues = model.Yvalues
        let N = xvalues.Length
        let n = N / 2 + 1
        let dx = (m.variables[t].B - m.variables[t].A) / float n

        let mutable x = m.variables[t].A
        m.variables[t].V <- ValueSome x
        xvalues[0] <- x
        yvalues[0] <- Evaluation.eval m t f

        let mutable i = 1
        while i + 1 < N do
            x <- x + dx
            m.variables[t].V <- ValueSome x
            let y = Evaluation.eval m t f
            xvalues[i + 0] <- x
            xvalues[i + 1] <- x 
            yvalues[i + 0] <- y
            yvalues[i + 1] <- y 
            i <- i + 2

        x <- m.variables[t].B
        m.variables[t].V <- ValueSome x
        xvalues[xvalues.Length - 1] <- x
        yvalues[yvalues.Length - 1] <- (Evaluation.eval m t f)           
        model.UpdateBounds()


    // let createTeXModel (cons:Map<string,float>) (vars:Map<string,Variable>) (fns:Map<string,Binder.BoundExpr>) tex t c s =
    let createTeXModel (maps:Maps) tex t c s :Model =
        let s': Symbols = {
            constants = maps.constants.Keys
            variables = maps.variables.Keys
            functions = maps.functions.Keys
        }
        let f = Parser.parse s' tex
        let b = Binder.bind f
        let x = Array.zeroCreate<float> 100
        let y = Array.zeroCreate<float> 100
        // let (Model2D m) = createline x y c s
        let m = match (createline x y c s) with | Model2D m -> m | _ -> failwith "improper type"
        evalModel maps t b m
        TeXModel (tex, f, b, m)


    let texModels (models:list<Model>) =
        models 
        |> Array.ofList 
        |> Array.filter (function | TeXModel _ -> true | _ -> false) 
        |> Array.map (function | TeXModel (tex,f,b,m) -> (tex,f,b,m) | _ -> failwith "not a tex-model")


    let setNames (names:list<string>) (models:list<Model>) = 
        for name, model in List.zip names models do
            match model with
            | Model2D m -> m.Name <- name
            | TeXModel (tex, f, b, m) -> m.Name <- name


    let rawModels (models:list<Model>) =
        models
        |> Array.ofList
        |> Array.filter (function | Model2D _ -> true | _ -> false)
        |> Array.map (function | Model2D m -> m | _ -> failwith "not a model2d")



module Views =
    open Avalonia.FuncUI.Builder
    open Avalonia.FuncUI.Types
    open Model2

    type ToggleButton with
        static member content<'t when 't :> ToggleButton>(value:obj) :IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<obj>(ToggleButton.ContentProperty, value, ValueNone)


    // let ComponentSlider 
    //     (id:string)
    //     (value:string * Variable) 
    //     (cons:Map<string,float>) 
    //     (vars:Map<string,Variable>) 
    //     (fns:Map<string,Binder.BoundExpr>) 
    //     (tex_models: array<string * ExprTree.Expr * Binder.BoundExpr * SKCharts.Model2D>)
    //     (target:IReadable<string>) 
    //     (chart:SKChart2D) =
    let ComponentSlider 
        (value:string * Variable) 
        (maps:Maps) 
        (tex_models: array<string * ExprTree.Expr * Binder.BoundExpr * SKCharts.Model2D>)
        (target:IReadable<string>) 
        (chart:SKChart2D) =
        Component(fun ctx ->
            let (s,v) = value
            let variable = ctx.useState (match v.V with | ValueSome _v -> _v | ValueNone -> v.A + (v.B - v.A) / 2.0)
            let target = ctx.usePassedRead target

            Grid.create [
                Grid.columnDefinitions "100, 140, 200"
                Grid.verticalScrollBarVisibility ScrollBarVisibility.Auto
                Grid.children [
                    Image.create [
                        Image.column 0
                        Image.stretch Media.Stretch.None
                        Image.horizontalAlignment HorizontalAlignment.Left
                        Image.source (Converter.convert s)
                    ]

                    Slider.create [
                        Slider.column 1
                        Slider.width 120
                        Slider.minimum v.A
                        Slider.maximum v.B
                        Slider.value variable.Current    
                        Slider.onValueChanged (fun d ->
                            maps.variables[s].V <- ValueSome d

                            for (tex, f, b, m) in tex_models do
                                if target.Current <> String.Empty && s <> target.Current && tex.Contains(s) then
                                    evalModel maps target.Current b m

                            chart.NormalizeModels()
                            variable.Set d
                        )
                    ]

                    TextBlock.create [
                        TextBlock.column 2
                        TextBlock.width 100
                        TextBlock.maxWidth 100
                        TextBlock.text (variable.Current.ToString("N4"))
                    ]
                ]
            ]
        )


    // let view2 (constants:Map<string,float>, variables:Map<string,Variable>, functions:Map<string,Binder.BoundExpr>, models:list<Model>) =
    let view2 (maps:Maps, models:list<Model>) =
        Component(fun ctx ->
            let tex_models = texModels models
            let raw_models = rawModels models            
            let c = new SKChart2DControl()
            for m in raw_models do c.Chart.AttachModel m
            for (tex, f, b, m) in tex_models do c.Chart.AttachModel m            
            
            let cons_strs = 
                tex_models
                |> Array.map (fun (tex, f, b, m) -> ExprTree.constants f)
                |> Array.reduce (fun x y -> Array.concat [x; y])
                |> Array.map (fun x -> x.Value)
                |> Array.distinct
        
            let symbols_strs = 
                tex_models
                |> Array.map (fun (tex, f, b, m) -> ExprTree.variables f)
                |> Array.reduce (fun x y -> Array.concat [x; y])
                |> Array.map (fun x -> x.Value)
                |> Array.distinct
            
            let _cons =
                cons_strs
                |> Array.filter (fun x -> maps.constants.ContainsKey x)
                |> Array.map (fun x -> x,maps.constants[x])

            let _vars = 
                symbols_strs 
                |> Array.filter (fun x -> maps.variables.ContainsKey x) 
                |> Array.map (fun x -> x,maps.variables[x])

            let _fns =
                symbols_strs
                |> Array.filter (fun x -> maps.functions.ContainsKey x)
                |> Array.map (fun x -> x,maps.functions[x])

            let cons = ctx.useState _cons
            let vars = ctx.useState _vars
            let fns = ctx.useState _fns            
            let target = ctx.useState String.Empty
            let notation = ctx.useState true
        
            DockPanel.create [
                DockPanel.lastChildFill true
                DockPanel.children [
                    StackPanel.create [
                        StackPanel.dock Dock.Left
                        StackPanel.margin (Thickness(10.0, 0.0, 0.0, 0.0))
                        StackPanel.children [

                            ToggleButton.create [
                                ToggleButton.content "notation"
                                ToggleButton.margin (Thickness(0., 20., 15., 20.))
                                ToggleButton.isChecked notation.Current
                                ToggleButton.onClick (fun _ -> notation.Set (not (notation.Current)))
                            ]
                            TextBlock.create [TextBlock.text "Constants:"]
                            ListBox.create [
                                ListBox.maxHeight 150
                                ListBox.verticalScrollBarVisibility ScrollBarVisibility.Auto
                                ListBox.horizontalScrollBarVisibility ScrollBarVisibility.Auto
                                ListBox.dataItems _cons
                                ListBox.itemTemplate (
                                    DataTemplateView<string * float>.create (fun (s, c) ->
                                        Grid.create [
                                            Grid.columnDefinitions "1*, 1*"
                                            Grid.children [
                                                Image.create [
                                                    Image.stretch Media.Stretch.None 
                                                    Image.column 0
                                                    Image.source (Converter.convert s)
                                                ]
                                                TextBlock.create [
                                                    TextBlock.column 1
                                                    TextBlock.text $"{c}"
                                                ]
                                            ]
                                        ]
                                    )
                                )
                            ]

                            Border.create [Border.height 20; Border.width 200]
                            TextBlock.create [TextBlock.text "Variables:"]
                            ListBox.create [
                                ListBox.maxHeight 150
                                ListBox.verticalScrollBarVisibility ScrollBarVisibility.Auto
                                ListBox.horizontalScrollBarVisibility ScrollBarVisibility.Auto
                                ListBox.dataItems _vars
                                ListBox.itemTemplate (
                                    DataTemplateView<string * Variable>.create (fun (s, v) -> 
                                        Grid.create [
                                            Grid.columnDefinitions "1*, 1*, 1*"
                                            Grid.children [
                                                Image.create [
                                                    Image.stretch Media.Stretch.None 
                                                    Image.column 0
                                                    Image.source (Converter.convert s)
                                                ]
                                                TextBlock.create [
                                                    TextBlock.column 1
                                                    TextBlock.text $"{v.A}"
                                                ]
                                                TextBlock.create [
                                                    TextBlock.column 2
                                                    TextBlock.text $"{v.B}"                                                    
                                                ]
                                            ]
                                        ]    
                                    )
                                )                               
                            ]                            
                            
                            Border.create [Border.height 20; Border.width 200]
                            TextBlock.create [TextBlock.text "functions:"]
                            ListBox.create [
                                ListBox.maxHeight 150
                                ListBox.verticalScrollBarVisibility ScrollBarVisibility.Auto
                                ListBox.horizontalScrollBarVisibility ScrollBarVisibility.Auto
                                ListBox.dataItems _fns
                                ListBox.itemTemplate (
                                    DataTemplateView<string * Binder.BoundExpr>.create (fun (s,fn) -> 
                                        match fn with
                                        | Binder.Assignment (l ,r) ->
                                            Image.create [
                                                Image.stretch Media.Stretch.None 
                                                Image.source (Converter.convert l)
                                            ] 
                                        | _ -> Image.create []
                                    )
                                )                               
                            ]                            
                            
                            Border.create [Border.height 20; Border.width 200]
                            TextBlock.create [TextBlock.text "select target:"]
                            ComboBox.create [
                                ComboBox.width 150
                                ComboBox.dataItems _vars
                                ComboBox.itemTemplate (
                                    DataTemplateView<string * Variable>.create (fun (s,v) ->
                                        Image.create [
                                            Image.stretch Media.Stretch.None 
                                            Image.source (Converter.convert s)
                                        ]        
                                    )
                                )
                                ComboBox.onSelectedItemChanged (fun x -> 
                                    if x <> null then
                                        let (t, v) = x :?> (string * Variable)                                        

                                        _vars
                                        |> Array.except [|(t, v)|]
                                        |> Array.map (fun (x, _) -> x,maps.variables[x])
                                        |> vars.Set

                                        target.Set t
                                        // when event triggers, sliders are created anew
                                        // restore the values of the variables map
                                        // to min to match sliders values
                                        _vars
                                        |> Seq.iter (fun (x, _) -> 
                                            maps.variables[x].V <- ValueSome (maps.variables[x].A + (maps.variables[x].B - maps.variables[x].A) / 2.)
                                        )
                                       
                                        for (tex, f, b, m) in tex_models do
                                            // if target <> String.Empty then
                                                evalModel maps target.Current b m

                                        c.Chart.NormalizeModels()
                                        c.Chart.XImg <- (Converter.image target.Current)        
                                )
                            ]
                            
                            Border.create [Border.height 20; Border.width 200]
                            Button.create [
                                Button.content "capture_vert"
                                Button.onClick (fun _ -> ())
                            ]                            
                        ]
                    ]
                    Grid.create [
                        Grid.dock Dock.Bottom
                        Grid.maxHeight 200
                        Grid.margin (Thickness(0.0, 0.0, 0.0, 10.0))
                        Grid.columnDefinitions "1*, 1*"
                        Grid.children [                            
                            ListBox.create [
                                ListBox.column 0
                                ListBox.verticalScrollBarVisibility ScrollBarVisibility.Auto
                                ListBox.horizontalScrollBarVisibility ScrollBarVisibility.Auto
                                // ListBox.dataItems [for i in vars.Current -> ComponentSlider $"slider-{i}" i constants variables functions tex_models target c.Chart]
                                ListBox.dataItems [for i in vars.Current -> ComponentSlider i maps tex_models target c.Chart]
                            ]
                            ListBox.create [
                                ListBox.column 1
                                ListBox.verticalScrollBarVisibility ScrollBarVisibility.Auto
                                ListBox.horizontalScrollBarVisibility ScrollBarVisibility.Auto
                                match notation.Current with
                                | true ->
                                    ListBox.dataItems (Array.map (fun (tex, _, _, _) -> tex) tex_models)
                                    ListBox.itemTemplate (
                                        DataTemplateView<string>.create(fun s ->
                                            Image.create [
                                                Image.stretch Media.Stretch.None
                                                Image.source (Converter.convert s)
                                            ]
                                        )
                                    )
                                | false ->
                                    ListBox.dataItems (Array.map (fun (tex, _, _, _) -> tex) tex_models)
                                    ListBox.itemTemplate (
                                        DataTemplateView<string>.create(fun s ->
                                            SelectableTextBlock.create [
                                                SelectableTextBlock.text s
                                            ]    
                                        )
                                    )                                
                            ]
                        ]
                    ]
                    ContentControl.create [
                        ContentControl.content c
                    ]                            
                ]
            ]
        )

    let viewTest() =
        Component(fun ctx -> 
            let tex = ctx.useState "f(x)"
            let textbox = ctx.useState<TextBox> null
            DockPanel.create [
                DockPanel.lastChildFill true
                DockPanel.children [
                    Button.create [
                        Button.dock Dock.Bottom
                        Button.content "convert"
                        Button.onClick (fun _ -> tex.Set textbox.Current.Text)
                    ]
                    TextBox.create [
                        TextBox.init textbox.Set
                        TextBox.dock Dock.Bottom
                        TextBox.height 300
                        TextBox.width 800                        
                    ]
                    Image.create [
                        Image.stretch Media.Stretch.None
                        Image.source (Converter.convert tex.Current)
                    ]
                ]
            ]    
        )

    type MainWindow() =
        inherit HostWindow()
        do
            base.Title <- "MKXK"
            // base.Content <- view2()
        

    type App() =
        inherit Application()

        override this.Initialize() =
            this.Styles.Add (FluentTheme())
            this.RequestedThemeVariant <- Styling.ThemeVariant.Light

        override this.OnFrameworkInitializationCompleted() =
            match this.ApplicationLifetime with
            | :? IClassicDesktopStyleApplicationLifetime as dsk -> dsk.MainWindow <- MainWindow()
            | _ -> ()

        
