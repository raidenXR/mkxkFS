namespace RendererFS

open Avalonia
open Avalonia.Controls.Primitives
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout

open System
open MKXK
open SkiaSharp
open SKCharts



module Converter =
    open Notation
    open Models
    open System.Linq

    let ms = new System.IO.MemoryStream(8 * 1024)

    let convert (transparent:bool) (tex:string) = 
        ms.Position <- 0
        let exprs = Parser.parseExprs tex
        Typesetting.render transparent ms exprs           
        ms.Position <- 0
        new Avalonia.Media.Imaging.Bitmap(ms)

    let convertSymbol (tex:string) = 
        ms.Position <- 0
        let exprs = Parser.parseExprs tex
        Typesetting.renderAlpha ms exprs           
        ms.Position <- 0
        new Avalonia.Media.Imaging.Bitmap(ms)

    let image (tex:string) =
        ms.Position <- 0
        let exprs = Parser.parseExprs tex
        Typesetting.render true ms exprs           
        ms.Position <- 0
        SKImage.FromEncodedData(ms)


module Views =
    open Avalonia.FuncUI.Builder
    open Avalonia.FuncUI.Types
    open Models

    type ToggleButton with
        static member content<'t when 't :> ToggleButton>(value:obj) :IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<obj>(ToggleButton.ContentProperty, value, ValueNone)

    type ContentControl with
        static member onSizeChanged<'t when 't :> ContentControl>(func: SizeChangedEventArgs -> unit, ?subPatchOptions) :IAttr<'t> =
            AttrBuilder<'t>.CreateSubscription<SizeChangedEventArgs>(ContentControl.SizeChangedEvent, func, ?subPatchOptions = subPatchOptions)
           
    type SKChartView =
        | Chart2 = 0
        | Chart3 = 1

    let SliderComponent 
        (value:string * Variable) 
        (maps:Maps) 
        (skchart:SKChartControl)
        (tx:ref<string>)
        (ty:ref<string>) =
        Component(fun ctx ->
            let (s,v) = value
            let variable = ctx.useState (match v.V with | ValueSome _v -> _v | ValueNone -> v.A + (v.B - v.A) / 2.0)

            Grid.create [
                Grid.columnDefinitions "100, 140, 200"
                Grid.verticalScrollBarVisibility ScrollBarVisibility.Auto
                Grid.children [
                    Image.create [
                        Image.column 0
                        Image.stretch Media.Stretch.None
                        Image.horizontalAlignment HorizontalAlignment.Left
                        Image.source (Converter.convertSymbol s)
                    ]
                    Slider.create [
                        Slider.column 1
                        Slider.width 120
                        Slider.minimum v.A
                        Slider.maximum v.B
                        Slider.value variable.Current    
                        Slider.onValueChanged (fun d ->
                            if maps.variables.ContainsKey s then
                                maps.variables[s].V <- ValueSome d

                                if skchart.IsSKChart2 then
                                    let c2 = skchart.AsSKChart2()
                                    for ob in skchart.Args do
                                        let (tex,i,b) = (ob :?> string * int * Binder.BoundExpr)
                                        evalModel2 maps tx.Value b (c2.Models[i])
                                    c2.Update()
                                if skchart.IsSKChart3 then
                                    let c3 = skchart.AsSKChart3()
                                    for ob in skchart.Args do
                                        let (tex,i,b) = (ob :?> string * int * Binder.BoundExpr)
                                        evalModel3 maps tx.Value ty.Value b (c3.Models[i])
                                    c3.Update()                            
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

    let ComboBoxComponent (
        tag:string, 
        maps:Maps, 
        vars:array<string * Variable>, 
        tout:ref<string>, 
        tin:ref<String>,
        skchart:SKChartControl, 
        models:seq<Model>,
        tex_fns:IWritable<list<string>>) =
        Component.create("combobox", fun ctx ->
            let tex_fns = ctx.usePassed tex_fns
            StackPanel.create [
                StackPanel.children [
                    TextBlock.create [TextBlock.text tag]
                    ComboBox.create [
                        ComboBox.width 150
                        ComboBox.dataItems vars
                        ComboBox.itemTemplate (
                            DataTemplateView<string * Variable>.create (fun (s,v) ->
                                Image.create [
                                    Image.stretch Media.Stretch.None 
                                    Image.source (Converter.convertSymbol s)
                                ]        
                            )
                        )
                        ComboBox.onSelectedItemChanged (fun args ->
                            if args <> null then
                                let (t,v) = args :?> (string * Variable) 
                                let variables = maps.variables

                                vars
                                |> Seq.iter (fun (x, _) -> variables[x].V <- ValueSome (variables[x].A + (variables[x].B - variables[x].A) / 2.))

                                if t <> String.Empty && variables.ContainsKey(t) then 
                                    tout.Value <- t
                                    if variables.ContainsKey(t) && variables.ContainsKey(tin.Value) then 
                                        let c3 = skchart.AsSKChart3()
                                        skchart.Args.Clear()
                                        c3.Models.Clear()
                                        let mutable i = 0
                                        for model in models do
                                            match model with
                                            | RawModel3 (tx,ty,tz,m) -> if tx = t && tin.Value = ty then c3.Models.Add(m)
                                            | TeXModel (tex,f,b,c,s) when tex.Contains(t) && tex.Contains(tin.Value) ->
                                                let m = Model3.createEmpty ChartType.Points 20 20 (SKColor(uint32 c)) s
                                                evalModel3 maps tout.Value tin.Value b m
                                                c3.Models.Add(m)
                                                let arg = (tex,i,b)
                                                skchart.Args.Add(arg) |> ignore
                                                i <- i + 1
                                            | _ -> ()
                                        c3.XImg <- Converter.image tout.Value 
                                        c3.YImg <- Converter.image tin.Value 
                                        c3.ResetBounds()
                                        c3.Update()
                                        // printfn "C3-bounds: %A" c3.Bounds
                                        
                                    elif variables.ContainsKey(t) then 
                                        let c2 = skchart.AsSKChart2() 
                                        skchart.Args.Clear()
                                        c2.Models.Clear()
                                        let mutable i = 0
                                        for model in models do
                                            match model with
                                            | RawModel2 (tx, ty, m) -> if tx = t then c2.Models.Add(m)
                                            | TeXModel (tex,f,b,c,s) when tex.Contains(t) -> 
                                                let (ExprTree.Assignment (n,e)) = f                                                
                                                let m = Model2.createEmpty ChartType.Line 100 (SKColor(uint32 c)) s
                                                evalModel2 maps t b m
                                                m.name <- n
                                                c2.Models.Add(m)
                                                let arg = (tex,i,b)
                                                skchart.Args.Add(arg) |> ignore
                                                i <- i + 1
                                            | _ -> ()
                                        c2.XImg <- Converter.image tout.Value 
                                        c2.ResetBounds()
                                        c2.Update()   
                                        // printfn "C2-bounds: %A" c2.Bounds
                                        
                                    else
                                        skchart.AsSKChart2() |> ignore

                                    // models
                                    // |> Seq.filter (function | TeXModel _ -> true | _ -> false)
                                    // |> Seq.map (function | TeXModel (tex,_,_,_,_) -> tex | _ -> failwith "...")
                                    // |> List.ofSeq 
                                    // |> tex_fns.Set

                        )
                    ]
                    Border.create [Border.height 20; Border.width 200]                    
                ]
            ] :> IView
        )

    // let FnNotationComponent (i:int) (s:string) (notation:IReadable<bool>) =
    let FnNotationComponent (s:string) (notation:IReadable<bool>) =
        Component(fun ctx ->
            let notation = ctx.usePassedRead notation
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.children [
                    TextBlock.create [
                        TextBlock.verticalAlignment VerticalAlignment.Center
                        TextBlock.width 40
                        // TextBlock.text ($"{i + 1}: ")
                    ]
                    match notation.Current with
                    | true -> 
                        Image.create [
                            Image.stretch Media.Stretch.None
                            Image.source (Converter.convert true s)
                        ]
                    | false ->
                        SelectableTextBlock.create [
                            SelectableTextBlock.text s
                        ]                      
                ]
            ]
            :> IView
        )
      

    let ConstantsView (constants:seq<string * float>) = 
        ListBox.create [
            ListBox.maxHeight 150
            ListBox.verticalScrollBarVisibility ScrollBarVisibility.Auto
            ListBox.horizontalScrollBarVisibility ScrollBarVisibility.Auto
            ListBox.dataItems constants
            ListBox.itemTemplate (
                DataTemplateView<string * float>.create (fun (s, c) ->
                    Grid.create [
                        Grid.columnDefinitions "1*, 1*"
                        Grid.children [
                            Image.create [
                                Image.stretch Media.Stretch.None 
                                Image.column 0
                                Image.source (Converter.convertSymbol s)
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
    
    let VariablesView (variables:seq<string * Variable>) =
        ListBox.create [
            ListBox.maxHeight 150
            ListBox.verticalScrollBarVisibility ScrollBarVisibility.Auto
            ListBox.horizontalScrollBarVisibility ScrollBarVisibility.Auto
            ListBox.dataItems variables
            ListBox.itemTemplate (
                DataTemplateView<string * Variable>.create (fun (s, v) -> 
                    Grid.create [
                        Grid.columnDefinitions "1*, 1*, 1*"
                        Grid.children [
                            Image.create [
                                Image.stretch Media.Stretch.None 
                                Image.column 0
                                Image.source (Converter.convertSymbol s)
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
        
    let FunctionsView (functions:seq<string * Binder.BoundExpr>) =
        ListBox.create [
            ListBox.maxHeight 150
            ListBox.verticalScrollBarVisibility ScrollBarVisibility.Auto
            ListBox.horizontalScrollBarVisibility ScrollBarVisibility.Auto
            ListBox.dataItems functions
            ListBox.itemTemplate (
                DataTemplateView<string * Binder.BoundExpr>.create (fun (s,fn) -> 
                    match fn with
                    | Binder.Assignment (l ,r) ->
                        Image.create [
                            Image.stretch Media.Stretch.None 
                            Image.source (Converter.convertSymbol l)
                        ] 
                    | _ -> Image.create []
                )
            )                               
        ]                            
    
    

    let view2 (maps:Maps, models':list<string * Model>) =
        Component(fun ctx ->
            let (names,models) = models' |> Array.ofList |> Array.unzip

            let tex_models =
                models 
                |> Array.filter (function | TeXModel _ -> true | _ -> false)
                |> Array.map (function | TeXModel (tex,f,b,c,s) -> (tex,f,b,c,s) | _ -> failwith "model is not texmodel")
            
            let cons_strs = 
                tex_models
                |> Array.map (fun (_,f,_,_,_) -> ExprTree.constants f)
                |> Array.reduce (fun x y -> Array.concat [x; y])
                |> Array.map (fun x -> x.Value)
                |> Array.distinct
        
            let symbols_strs = 
                tex_models
                |> Array.map (fun (_,f,_,_,_) -> ExprTree.variables f)
                |> Array.reduce (fun x y -> Array.concat [x; y])
                |> Array.map (fun x -> x.Value)
                |> Array.distinct
            
            let _cons =
                cons_strs
                |> Array.filter (maps.constants.ContainsKey)
                |> Array.map (fun x -> x,maps.constants[x])

            let _vars = 
                symbols_strs 
                |> Array.filter (maps.variables.ContainsKey) 
                |> Array.map (fun x -> x,maps.variables[x])

            let _fns =
                symbols_strs
                |> Array.filter (fun x -> maps.functions.ContainsKey x)
                |> Array.map (fun x -> x,maps.functions[x])

            let _tex_strs_i = 
                tex_models
                |> Array.map (fun (tex,_,_,_,_) -> tex)
                |> List.ofArray
                // |> Array.indexed

            let target_x = ref String.Empty
            let target_y = ref String.Empty
            
            let notation = ctx.useState true
            let tex_fns  = ctx.useState _tex_strs_i
            let skchart = new SKChartControl()
        
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
                            
                            if _cons.Length > 0 then
                                TextBlock.create [TextBlock.text "Constants:"]
                                ConstantsView _cons
                                Border.create [Border.height 20; Border.width 200]

                            if _vars.Length > 0 then 
                                TextBlock.create [TextBlock.text "Variables:"]
                                VariablesView _vars
                                Border.create [Border.height 20; Border.width 200]
                            
                            if _fns.Length > 0 then
                                TextBlock.create [TextBlock.text "functions:"]
                                FunctionsView _fns
                                Border.create [Border.height 20; Border.width 200]

                            ComboBoxComponent("select target-x", maps, _vars, target_x, target_y, skchart, models, tex_fns)
                            ComboBoxComponent("select target-y", maps, _vars, target_y, target_x, skchart, models, tex_fns)

                            Slider.create [
                                Slider.margin (Thickness(0.,0.5))
                                Slider.width 120
                                Slider.minimum -45
                                Slider.maximum 45
                                Slider.value 0
                                Slider.onValueChanged(fun v -> 
                                    if skchart.IsSKChart3 then
                                        let c3 = skchart.AsSKChart3()
                                        c3.Camera.Elevation <- float32 v
                                        c3.Update()
                                )
                            ]
                            Slider.create [
                                Slider.margin (Thickness(0.,0.5))
                                Slider.width 120
                                Slider.minimum -90
                                Slider.maximum 90
                                Slider.value 0
                                Slider.onValueChanged(fun v -> 
                                    if skchart.IsSKChart3 then
                                        let c3 = skchart.AsSKChart3()
                                        c3.Camera.Azimuth <- float32 v
                                        c3.Update()
                                )
                            ]
                        ]
                    ]
                    // right-side of the panel, that contains the mathematical notations
                    Grid.create [
                        Grid.dock Dock.Right
                        Grid.maxWidth 340
                        Grid.margin (Thickness(0.0, 0.0, 10.0, 10.0))
                        Grid.children [
                            ListBox.create [
                                ListBox.background "White"
                                ListBox.column 1
                                ListBox.horizontalScrollBarVisibility ScrollBarVisibility.Auto
                                ListBox.verticalScrollBarVisibility ScrollBarVisibility.Auto
                                ListBox.dataItems [for tex in tex_fns.Current -> FnNotationComponent tex notation]
                            ]                            
                        ]
                    ]
                    // bottom-side of the panel, that contains the sliders
                    Grid.create [
                        Grid.dock Dock.Bottom
                        Grid.maxHeight 200
                        Grid.margin (Thickness(0.0, 0.0, 10.0, 10.0))
                        Grid.children [                            
                            ListBox.create [
                                ListBox.column 0
                                ListBox.background "White"
                                ListBox.verticalScrollBarVisibility ScrollBarVisibility.Auto
                                ListBox.horizontalScrollBarVisibility ScrollBarVisibility.Auto
                                ListBox.dataItems [for s in _vars -> SliderComponent s maps skchart target_x target_y]
                            ]
                        ]
                    ]
                    ContentControl.create [ContentControl.content skchart]
                ]
            ]
        )


    type MainWindow() =
        inherit HostWindow()
        do
            base.Title <- "MKXK"
        

    type App() =
        inherit Application()

        override this.Initialize() =
            this.Styles.Add (FluentTheme())
            this.RequestedThemeVariant <- Styling.ThemeVariant.Light

        override this.OnFrameworkInitializationCompleted() =
            match this.ApplicationLifetime with
            | :? IClassicDesktopStyleApplicationLifetime as dsk -> dsk.MainWindow <- MainWindow()
            | _ -> ()

        
