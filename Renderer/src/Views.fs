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
    open Notation
    open System.Linq

    let ms = new System.IO.MemoryStream(8 * 1024)

    let convert (tex:string) = 
        ms.Position <- 0
        let parser = Parser(tex)
        let hlist = parser.Parse().ToList()
        use renderer = new TeXRenderer(16.0f)
        renderer.Typeset hlist
        renderer.Render(ms, hlist)
        ms.Position <- 0
        new Avalonia.Media.Imaging.Bitmap(ms)


    let image (tex:string) =
        ms.Position <- 0
        let parser = Parser(tex)
        let hlist = parser.Parse().ToList()
        use renderer = new TeXRenderer(16.0f)
        renderer.Typeset hlist
        renderer.Render(ms, hlist)
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

    let evalModel (cons:Map<string,float>) (vars:Map<string,Variable>) (fns:Map<string,Binder.BoundExpr>) t f (m:SKCharts.Model2D) =
        let xvalues = m.Xvalues
        let yvalues = m.Yvalues
        let N = xvalues.Length
        let n = N / 2 + 2
        let dx = (vars[t].B - vars[t].A) / float n

        let mutable x = vars[t].A
        vars[t].V <- ValueSome x
        xvalues[0] <- x
        yvalues[0] <- Evaluation.eval cons vars fns t f

        let mutable i = 1
        while i + 1 < N do
            x <- x + dx
            vars[t].V <- ValueSome x
            let y = Evaluation.eval cons vars fns t f
            xvalues[i + 0] <- x
            xvalues[i + 1] <- x 
            yvalues[i + 0] <- y
            yvalues[i + 1] <- y 
            i <- i + 2

        x <- vars[t].B
        vars[t].V <- ValueSome x
        xvalues[xvalues.Length - 1] <- x
        yvalues[yvalues.Length - 1] <- (Evaluation.eval cons vars fns t f)           
        m.UpdateBounds()


    let createTeXModel (cons:Map<string,float>) (vars:Map<string,Variable>) (fns:Map<string,Binder.BoundExpr>) tex t (x:array<float>) c s =
        let f = Parser.parse tex cons.Keys vars.Keys fns.Keys
        let b = Binder.bind f
        let y = Array.zeroCreate<float> x.Length
        let m = match (createline x y c s) with | Model2D m -> m | _ -> failwith "improper type"
        // evalModel cons vars fns t b m
        TeXModel (tex, f, b, m)


    let texModels (models:list<Model>) =
        models 
        |> Array.ofList 
        |> Array.filter (function | TeXModel _ -> true | _ -> false) 
        |> Array.map (function | TeXModel (tex,f,b,m) -> (tex,f,b,m) | _ -> failwith "not a tex-model")

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


    let view2 (constants:Map<string,float>, variables:Map<string,Variable>, functions:Map<string,Binder.BoundExpr>, models:list<Model>) =
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
                |> Array.filter (fun x -> constants.ContainsKey x)
                |> Array.map (fun x -> x,constants[x])

            let _vars = 
                symbols_strs 
                |> Array.filter (fun x -> variables.ContainsKey x) 
                |> Array.map (fun x -> x,variables[x])

            let _fns =
                symbols_strs
                |> Array.filter (fun x -> functions.ContainsKey x)
                |> Array.map (fun x -> x,functions[x])

            let cons = ctx.useState _cons
            let vars = ctx.useState _vars
            let fns = ctx.useState _fns            
            let mutable target = String.Empty
        
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
                                ToggleButton.isChecked true
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
                                        
                                        target <- t
                                        c.Chart.XImg <- (Converter.image target)        

                                        _vars
                                        |> Array.except [|(t, v)|]
                                        |> Array.map (fun (x, _) -> x,variables[x])
                                        |> vars.Set

                                        // when event triggers, sliders are created anew
                                        // restore the values of the variables map
                                        // to min to match sliders values
                                        vars.Current
                                        |> Seq.iter (fun (x, _) -> variables[x].V <- ValueSome variables[x].A)
                                       
                                        for (tex, f, b, m) in tex_models do
                                            if target <> String.Empty then
                                                evalModel constants variables functions target b m

                                        c.Chart.NormalizeModels()
                                )
                            ]
                            
                            Border.create [Border.height 20; Border.width 200]
                            Button.create [
                                Button.content "capture_vert"
                                Button.onClick (fun _ -> ())
                            ]                            
                        ]
                    ]

                    // Border.create [Border.dock Dock.Top; Border.height 20; Border.width 200]
                    // TextBlock.create []
                    // Image.create [
                    //     // Image.source (Converter.convert (Seq.last ))
                    // ]
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
                                ListBox.dataItems vars.Current
                                ListBox.itemTemplate (
                                    DataTemplateView<string * Variable>.create (fun (s, v) -> 
                                        // let _slider = ctx.useState<Slider> null
                                        Grid.create [
                                            Grid.columnDefinitions "100, 140, 200"
                                            Grid.children [
                                                Image.create [
                                                    Image.column 0
                                                    Image.stretch Media.Stretch.None 
                                                    Image.horizontalAlignment HorizontalAlignment.Left
                                                    Image.source (Converter.convert s)
                                                ]
                                                // View.createWithOutlet _slider.Set Slider.create [
                                                    // Slider.init _slider.Set
                                                Slider.create [
                                                    Slider.column 1
                                                    Slider.width 100
                                                    Slider.minimum v.A
                                                    Slider.maximum v.B    
                                                    Slider.onValueChanged (fun d ->
                                                        variables[s].V <- ValueSome d

                                                        for (tex, f, b, m) in tex_models do
                                                            if target <> String.Empty && tex.Contains(s) then
                                                                evalModel constants variables functions target b m
                                                        c.Chart.NormalizeModels() 
                                                    )
                                                ]
                                                TextBlock.create [
                                                    TextBlock.column 2
                                                    TextBlock.width 100
                                                    TextBlock.maxWidth 100
                                                    // TextBlock.text (string _slider.Current.Value)
                                                ]
                                            ]
                                        ]
                                    )
                                )
                            ]

                            ListBox.create [
                                ListBox.column 1
                                ListBox.verticalScrollBarVisibility ScrollBarVisibility.Auto
                                ListBox.horizontalScrollBarVisibility ScrollBarVisibility.Auto
                                ListBox.dataItems (Array.map (fun (tex, _, _, _) -> tex) tex_models)
                                ListBox.itemTemplate (
                                    DataTemplateView<string>.create(fun s ->
                                        Image.create [
                                            Image.stretch Media.Stretch.None
                                            Image.source (Converter.convert s)
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

        
