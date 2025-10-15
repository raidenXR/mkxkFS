namespace MKXK.Viewer

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
open Avalonia.Rendering
open Avalonia.VisualTree

open System
open MKXK
open SkiaSharp
open SKCharts
open SKCharts.Avalonia


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


    let private c2 = new SKChart2([], W = 900f, H = 680f)
    let private c3 = new SKChart3([], Colormap.Hot, W = 900f, H = 680f)
    let private skchart = new SKChart(c2,c3)


    let SliderComponent 
        (value:string * Variable)
        (maps:Maps) 
        (target_x:IReadable<string>)
        (target_y:IReadable<string>) = Component(fun ctx ->
            let tx = ctx.usePassedRead target_x
            let ty = ctx.usePassedRead target_y
            let (s,v) = value
            let V = match v.V with | ValueSome _v -> _v | ValueNone -> v.A + (v.B - v.A) / 2.0
            let slider_value = ctx.useState V
            
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
                        Slider.value V
                        Slider.onValueChanged (fun d ->
                            let variables = maps.variables
                            
                            if s <> String.Empty && variables.ContainsKey s then
                                variables[s].V <- ValueSome d
                                let tx = target_x.Current
                                let ty = target_y.Current

                                if tx <> String.Empty && variables.ContainsKey(tx) && ty <> String.Empty && variables.ContainsKey(ty) then
                                    let c3 = skchart.AsSKChart3()
                                    for ob in skchart.Args do
                                        let (tex,i,b) = (ob :?> string * int * Binder.BoundExpr)
                                        // if not (maps.variables.ContainsKey(tx)) then printfn "maps.variables not contain x: -%s-" tx
                                        // if not (maps.variables.ContainsKey(ty)) then printfn "maps.variables not contain y: -%s-" ty
                                        evalModel3 maps tx ty b (c3.Models[i])
                                    c3.UpdateCachedBounds()                            
                                elif tx <> String.Empty && variables.ContainsKey(tx) then
                                    let c2 = skchart.AsSKChart2()
                                    for ob in skchart.Args do
                                        let (tex,i,b) = (ob :?> string * int * Binder.BoundExpr)
                                        // if not (maps.variables.ContainsKey(tx)) then printfn "maps.variables not contain x: -%s-" tx
                                        evalModel2 maps tx b (c2.Models[i])
                                    c2.UpdateCachedBounds()

                                slider_value.Set d
                        )
                    ]
                    TextBlock.create [
                        TextBlock.column 2
                        TextBlock.width 100
                        TextBlock.maxWidth 100
                        TextBlock.text (slider_value.Current.ToString("N4"))
                    ]
                ]
            ]
        )
        

    let FnNotationComponent (i:int) (s:string) (notation:IReadable<bool>) = Component(fun ctx -> 
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                TextBlock.create [
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.width 40
                    TextBlock.text ($"{i + 1}: ")
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
    
    let count_models tx ty (models:array<Model>) =
        let mutable n = 0
        for model in models do
            match model with
            | RawModel3 (_tx,_ty,_,m) -> if _tx = tx && ty = _ty then n <- n + 1
            | TeXModel (tex,f,b,c,s) when tex.Contains(tx) && tex.Contains(ty) -> n <- n + 1
            | _ -> ()
        n
    

    let view2 (maps:Maps, models':list<string * Model>) =
        Component(fun ctx ->
            let (names,models) = models' |> Array.ofList |> Array.unzip
            let empty_var = (String.Empty, {A = 0.0; B = 0.1; V = ValueNone})

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
                |> Array.append [|empty_var|] 

            let _fns =
                symbols_strs
                |> Array.filter (fun x -> maps.functions.ContainsKey x)
                |> Array.map (fun x -> x,maps.functions[x])

            let _tex_strs_i = 
                tex_models
                |> Array.map (fun (tex,_,_,_,_) -> tex)
                // |> List.ofArray
                // |> Array.indexed

            let target_x = ctx.useState String.Empty
            let target_y = ctx.useState String.Empty
            
            let notation = ctx.useState true
            let tex_fns  = ctx.useState _tex_strs_i
            let vars_list = ctx.useState _vars
            let skchart_state = ctx.useState skchart
        
            DockPanel.create [
                DockPanel.lastChildFill true
                DockPanel.children [
                    Menu.create [
                        Menu.dock Dock.Top
                        Menu.borderBrush "Silver"
                        Menu.borderThickness 1
                        Menu.margin (Thickness(0,0,0,10))
                        Menu.viewItems [
                            MenuItem.create [
                                MenuItem.header "notation"
                                MenuItem.onClick (fun _ -> notation.Set (not (notation.Current)))
                            ]
                            MenuItem.create [
                                MenuItem.header "capture"
                                MenuItem.viewItems [
                                    MenuItem.create [
                                        MenuItem.header "vertices"    
                                        MenuItem.onClick (fun _ ->
                                            if not (System.IO.Directory.Exists("vertices")) then 
                                                System.IO.Directory.CreateDirectory("vertices") |> ignore
                                        
                                            let name = System.Guid.NewGuid().ToString()
                                            let path = System.IO.Path.Combine("vertices", name)
                                            use fs = System.IO.File.CreateText(path)
                                            if skchart.IsSKChart2 then
                                                let c2 = skchart.AsSKChart2()
                                                for m in c2.Models do
                                                    fs.WriteLine("")
                                                    fs.WriteLine("")
                                                    for i in 0..m.xvalues.Length - 1 do
                                                        fs.WriteLine($"{m.xvalues[i]}  {m.yvalues[i]}")
                                            if skchart.IsSKChart3 then
                                                let c3 = skchart.AsSKChart3()
                                                for m in c3.Models do
                                                    fs.WriteLine("")
                                                    fs.WriteLine("")
                                                    for i in 0..m.xvalues.Length - 1 do
                                                        fs.WriteLine($"{m.xvalues[i]}  {m.yvalues[i]}  {m.zvalues[i]}")                                        
                                        )
                                    ]
                                    MenuItem.create [
                                        MenuItem.header "variables"
                                        MenuItem.onClick (fun _ ->
                                            if not (System.IO.Directory.Exists("variables")) then
                                                System.IO.Directory.CreateDirectory("variables") |> ignore

                                            let name = System.Guid.NewGuid().ToString()
                                            let path = System.IO.Path.Combine("variables", name)
                                            use fs = System.IO.File.CreateText(path)
                                            for tex in tex_fns.Current do
                                                fs.WriteLine tex

                                            fs.WriteLine("")
                                            fs.WriteLine("")
                                            for (s,v) in vars_list.Current do
                                                if s <> String.Empty && maps.variables.ContainsKey(s) then
                                                    let V = match v.V with | ValueSome V -> V | ValueNone -> v.A + (v.B - v.A) / 2.
                                                    fs.WriteLine($"{s}: {V}")                                                
                                        )
                                    ]
                                ]
                            ]
                        ]
                    ]
                    StackPanel.create [
                        StackPanel.dock Dock.Left
                        StackPanel.margin (Thickness(10.0, 0.0, 0.0, 0.0))
                        StackPanel.verticalScrollBarVisibility ScrollBarVisibility.Auto
                        StackPanel.horizontalScrollBarVisibility ScrollBarVisibility.Auto
                        StackPanel.children [                            
                            if _cons.Length > 0 then
                                TextBlock.create [TextBlock.text "Constants:"]
                                ConstantsView _cons
                                Border.create [Border.height 20; Border.width 200]

                            if _vars.Length > 0 then 
                                TextBlock.create [TextBlock.text "Variables:"]
                                VariablesView (Array.except [|empty_var|] _vars)
                                Border.create [Border.height 20; Border.width 200]
                            
                            if _fns.Length > 0 then
                                TextBlock.create [TextBlock.text "functions:"]
                                FunctionsView _fns
                                Border.create [Border.height 20; Border.width 200]

                            TextBlock.create [TextBlock.text "select target-x: "]
                            ComboBox.create [
                                ComboBox.width 150
                                ComboBox.dataItems _vars
                                ComboBox.itemTemplate (
                                    DataTemplateView<string * Variable>.create (fun (s,_) ->
                                        Image.create [
                                            Image.stretch Media.Stretch.None
                                            Image.source (Converter.convertSymbol s)
                                        ]
                                    )
                                )
                                ComboBox.onSelectedItemChanged (fun args ->
                                    if args <> null then
                                        let (tx,v) = args :?> (string * Variable)
                                        let ty = target_y.Current
                                        let variables = maps.variables

                                        _vars
                                        |> Seq.iter (fun (x, _) -> if variables.ContainsKey(x) then variables[x].V <- ValueSome (variables[x].A + (variables[x].B - variables[x].A) / 2.))

                                        if variables.ContainsKey(tx) && variables.ContainsKey(ty) then
                                            let c3 = skchart.AsSKChart3()
                                            skchart.Args.Clear()
                                            c3.Models.Clear()
                                            c3.ResetBounds()
                                            let n = count_models tx ty models
                                            let mutable i = 0
                                            for model in models do
                                                match model with
                                                | RawModel3 (_tx,_ty,_,m) -> if _tx = tx && ty = _ty then c3.Models.Add(m)
                                                | TeXModel (tex,f,b,c,s) when tex.Contains(tx) && tex.Contains(ty) ->
                                                    let m = Model3.createEmpty (if n = 1 then ChartType.Surface else ChartType.Points) 40 40 (SKColor(uint32 c)) s
                                                    evalModel3 maps tx ty b m
                                                    c3.Models.Add(m)
                                                    let arg = (tex,i,b)
                                                    skchart.Args.Add(arg) |> ignore
                                                    i <- i + 1
                                                | _ -> ()
                                            c3.XImg <- Converter.image tx
                                            c3.YImg <- Converter.image ty
                                            c3.Update()

                                        if variables.ContainsKey(tx) && ty = String.Empty then
                                            let c2 = skchart.AsSKChart2()
                                            skchart.Args.Clear()
                                            c2.Models.Clear()
                                            c2.ResetBounds()
                                            let mutable i = 0
                                            for model in models do
                                                match model with
                                                | RawModel2 (_tx,_,m) -> if _tx = tx then c2.Models.Add(m)
                                                | TeXModel (tex,f,b,c,s) when tex.Contains(tx) ->
                                                    let (ExprTree.Assignment (n,e)) = f
                                                    let m = Model2.createEmpty ChartType.Line 100 (SKColor(uint32 c)) s
                                                    evalModel2 maps tx b m
                                                    m.name <- n
                                                    c2.Models.Add(m)
                                                    let arg = (tex,i,b)
                                                    skchart.Args.Add(arg) |> ignore
                                                    i <- i + 1
                                                | _ -> ()
                                            c2.XImg <- Converter.image tx
                                            c2.Update()

                                        if tx = String.Empty && ty <> String.Empty then
                                            ignore ()   // ignore this case, tx must be set first 

                                        if tx = String.Empty && ty = String.Empty then
                                            let c2 = skchart.AsSKChart2()
                                            skchart.Args.Clear()
                                            c2.Models.Clear()
                                            c2.ResetBounds()
                                            c2.Update()
                                            c2.XImg <- null

                                        target_x.Set tx

                                        _tex_strs_i
                                        |> Array.filter (function tex -> tex.Contains(tx) && tex.Contains(ty))
                                        |> tex_fns.Set 

                                        _tex_strs_i
                                        |> Array.filter (function tex -> tex.Contains(tx) && tex.Contains(ty))
                                        |> Array.allPairs _vars
                                        |> Array.filter (fun ((t,v),tex) -> tex.Contains(t))
                                        |> Array.distinct
                                        |> Array.filter (fun ((t,v),tex) -> t <> tx && t <> ty)
                                        |> Array.map (fun ((t,v),tex) -> (t,v))
                                        |> vars_list.Set
                                        
                                )
                            ]

                            TextBlock.create [TextBlock.text "select target-y: "]
                            ComboBox.create [
                                ComboBox.width 150
                                ComboBox.dataItems _vars
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
                                        let (ty,v) = args :?> (string * Variable)
                                        let tx = target_x.Current
                                        let variables = maps.variables
                                        
                                        _vars
                                        |> Seq.iter (fun (x, _) -> if variables.ContainsKey(x) then variables[x].V <- ValueSome (variables[x].A + (variables[x].B - variables[x].A) / 2.))

                                        if variables.ContainsKey(tx) && variables.ContainsKey(ty) then
                                            let c3 = skchart.AsSKChart3()
                                            skchart.Args.Clear()
                                            c3.Models.Clear()
                                            c3.ResetBounds()
                                            let n = count_models tx ty models
                                            let mutable i = 0
                                            for model in models do
                                                match model with
                                                | RawModel3 (_tx,_ty,_,m) -> if _tx = tx && ty = _ty then c3.Models.Add(m)
                                                | TeXModel (tex,f,b,c,s) when tex.Contains(tx) && tex.Contains(ty) ->
                                                    let m = Model3.createEmpty (if n = 1 then ChartType.Surface else ChartType.Points) 40 40 (SKColor(uint32 c)) s
                                                    evalModel3 maps tx ty b m
                                                    c3.Models.Add(m)
                                                    let arg = (tex,i,b)
                                                    skchart.Args.Add(arg) |> ignore
                                                    i <- i + 1
                                                | _ -> ()
                                            c3.XImg <- Converter.image tx
                                            c3.YImg <- Converter.image ty
                                            c3.Update()

                                        if variables.ContainsKey(tx) && ty = String.Empty then
                                            let c2 = skchart.AsSKChart2()
                                            skchart.Args.Clear()
                                            c2.Models.Clear()
                                            c2.ResetBounds()
                                            let mutable i = 0
                                            for model in models do
                                                match model with
                                                | RawModel2 (_tx,_,m) -> if _tx = tx then c2.Models.Add(m)
                                                | TeXModel (tex,f,b,c,s) when tex.Contains(tx) ->
                                                    let (ExprTree.Assignment (n,e)) = f
                                                    let m = Model2.createEmpty ChartType.Line 100 (SKColor(uint32 c)) s
                                                    evalModel2 maps tx b m
                                                    m.name <- n
                                                    c2.Models.Add(m)
                                                    let arg = (tex,i,b)
                                                    skchart.Args.Add(arg) |> ignore
                                                    i <- i + 1
                                                | _ -> ()
                                            c2.XImg <- Converter.image tx
                                            c2.Update()

                                        if tx = String.Empty && ty <> String.Empty then
                                            ignore ()   // ignore this case, tx must be set first 
                                            
                                        if tx = String.Empty && ty = String.Empty then
                                            let c2 = skchart.AsSKChart2()
                                            skchart.Args.Clear()
                                            c2.Models.Clear()
                                            c2.ResetBounds()
                                            c2.Update()
                                            c2.XImg <- null
                                        
                                        target_y.Set ty

                                        _tex_strs_i
                                        |> Array.filter (function tex -> tex.Contains(tx) && tex.Contains(ty))
                                        |> tex_fns.Set 

                                        _tex_strs_i
                                        |> Array.filter (function tex -> tex.Contains(tx) && tex.Contains(ty))
                                        |> Array.allPairs _vars
                                        |> Array.filter (fun ((t,v),tex) -> tex.Contains(t))
                                        |> Array.distinct
                                        |> Array.filter (fun ((t,v),tex) -> t <> tx && t <> ty)
                                        |> Array.map (fun ((t,v),tex) -> (t,v))
                                        |> vars_list.Set
                                )
                            ]
                            
                            if skchart.IsSKChart3 then
                                // slider for c3 rotation - elevation
                                TextBlock.create [TextBlock.text "elevation: "]
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
                                            c3.UpdateCachedBounds()
                                    )
                                ]
                                // slider for c3 rotation - azimuth
                                TextBlock.create [TextBlock.text "azimuth: "]
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
                                            c3.UpdateCachedBounds()
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
                                ListBox.dataItems [for (i,tex_fn) in (Array.indexed tex_fns.Current) -> FnNotationComponent i tex_fn notation]
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
                                ListBox.dataItems [for s in (Array.except [empty_var] vars_list.Current) -> SliderComponent s maps target_x target_y]
                            ]
                        ]
                    ]


                    View.createGeneric<SKChartControl> []
                    |> View.withConstructorArgs [|skchart :> obj|]
                    // SKChartControl.create [
                    //     SKChartControl.chart skchart_state.Current
                    //     SKChartControl.onSizeChanged (fun s ->
                    //         if skchart.IsSKChart2 then 
                    //             let c2 = skchart.AsSKChart2()
                    //             c2.W <- float32 s.NewSize.Width
                    //             c2.H <- float32 s.NewSize.Height
                    //         elif skchart.IsSKChart3 then
                    //             let c3 = skchart.AsSKChart3()
                    //             c3.W <- float32 s.NewSize.Width
                    //             c3.H <- float32 s.NewSize.Height
                    //     )
                    // ]                    

                ]
            ]
        )


    type MainWindow() =
        inherit HostWindow()
        do
            base.Title <- "MainWindow"
        

    type App() =
        inherit Application()

        override this.Initialize() =
            this.Styles.Add (FluentTheme())
            this.RequestedThemeVariant <- Styling.ThemeVariant.Light

        override this.OnFrameworkInitializationCompleted() =
            match this.ApplicationLifetime with
            | :? IClassicDesktopStyleApplicationLifetime as dsk -> dsk.MainWindow <- MainWindow()
            | _ -> ()

        
