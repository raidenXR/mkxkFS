﻿namespace RendererFS

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

    type SKChartControl with
        static member create(attrs: IAttr<SKChartControl> list): IView<SKChartControl> =
            ViewBuilder.Create<SKChartControl>(attrs)

        static member chartProperty : StyledProperty<SKChart> =
            AvaloniaProperty.Register<SKChartControl, SKChart>("SKChart")
           

        static member backgroundProperty : StyledProperty<SKColor> =
            AvaloniaProperty.Register<SKChartControl, SKColor>("Background")
            
        static member chart<'t when 't :> SKChartControl>(value:SKChart) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<SKChart>(SKChartControl.chartProperty, value, ValueNone)
           
        static member background<'t when 't :> SKChartControl>(value:SKColor) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<SKColor>(SKChartControl.backgroundProperty, value, ValueNone)

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

    let ComboBoxComponent (
        tag:string, 
        maps:Maps, 
        vars:array<string * Variable>, 
        tout:IWritable<string>, 
        tin:IReadable<String>,
        models:seq<Model>,
        vars_list:IWritable<list<string * Variable>>,
        tex_fns:IWritable<list<string>>) = Component.create("combobox", fun ctx ->
            let vars_list = ctx.usePassed vars_list
            let tex_fns = ctx.usePassed tex_fns
            let tout = ctx.usePassed tout
            let tin  = ctx.usePassedRead tin
            
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
                                |> Seq.iter (fun (x, _) -> if variables.ContainsKey(x) then variables[x].V <- ValueSome (variables[x].A + (variables[x].B - variables[x].A) / 2.))

                                if variables.ContainsKey(t) then 
                                    let tex_strs = 
                                        models
                                        |> Seq.filter (function | TeXModel _ -> true | _ -> false)
                                        |> Seq.map (function | TeXModel (tex,_,_,_,_) -> tex | _ -> failwith "...")
                                    
                                    if variables.ContainsKey(t) && variables.ContainsKey(tin.Current) then 
                                        let c3 = skchart.AsSKChart3()
                                        let _t = tin.Current    // tx
                                        skchart.Args.Clear()
                                        c3.Models.Clear()
                                        c3.ResetBounds()
                                        let mutable i = 0
                                        for model in models do
                                            match model with
                                            | RawModel3 (tx,ty,tz,m) -> if tx = t && tin.Current = ty then c3.Models.Add(m)
                                            | TeXModel (tex,f,b,c,s) when tex.Contains(t) && tex.Contains(tin.Current) ->
                                                let m = Model3.createEmpty ChartType.Points 40 40 (SKColor(uint32 c)) s
                                                evalModel3 maps _t t b m
                                                c3.Models.Add(m)
                                                let arg = (tex,i,b)
                                                skchart.Args.Add(arg) |> ignore
                                                i <- i + 1
                                            | _ -> ()
                                        c3.XImg <- Converter.image _t
                                        c3.YImg <- Converter.image t
                                        c3.Update()
                                        // printfn "C3-bounds: %A" c3.Bounds
                                        let tex_strs_subset = tex_strs |> Seq.filter (function tex -> tex.Contains(t) && tex.Contains(tin.Current))
                                        let vars = vars |> Array.filter (fun (s,v) -> s <> t && s <> tin.Current)
                                        let vars_subset = [
                                            for tex in tex_strs_subset do
                                                for (s0,v0) in vars do
                                                    if tex.Contains(s0) then yield (s0,v0)
                                        ]
                                        tout.Set t
                                        vars_subset |> vars_list.Set                                     
                                        tex_strs_subset |> List.ofSeq |> tex_fns.Set
                                        
                                    elif variables.ContainsKey(t) then 
                                        let c2 = skchart.AsSKChart2() 
                                        skchart.Args.Clear()
                                        c2.Models.Clear()
                                        c2.ResetBounds()
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
                                        c2.XImg <- Converter.image t
                                        c2.Update()   
                                        // printfn "C2-bounds: %A" c2.Bounds                                        
                                        let tex_strs_subset = tex_strs |> Seq.filter (function tex -> tex.Contains(t))
                                        let vars = vars |> Array.filter (fun (s,v) -> s <> t && s <> tin.Current)
                                        let vars_subset = [
                                            for tex in tex_strs_subset do
                                                for (s0,v0) in vars do
                                                    if tex.Contains(s0) then yield (s0,v0)
                                        ]
                                        tout.Set t
                                        vars_subset |> vars_list.Set                                     
                                        tex_strs_subset |> List.ofSeq |> tex_fns.Set
                                
                                if t = String.Empty then
                                    if tin.Current = String.Empty then
                                        let c2 = skchart.AsSKChart2() 
                                        skchart.Args.Clear()
                                        c2.Models.Clear()
                                        c2.ResetBounds()
                                        c2.Update()
                                        c2.XImg <- null
                                        tout.Set t
                                    elif tin.Current <> String.Empty then
                                        let tex_strs = 
                                            models
                                            |> Seq.filter (function | TeXModel _ -> true | _ -> false)
                                            |> Seq.map (function | TeXModel (tex,_,_,_,_) -> tex | _ -> failwith "...")
                                        let _t = tin.Current
                                        let c2 = skchart.AsSKChart2() 
                                        skchart.Args.Clear()
                                        c2.Models.Clear()
                                        c2.ResetBounds()
                                        let mutable i = 0
                                        for model in models do
                                            match model with
                                            | RawModel2 (tx, ty, m) -> if tx = _t then c2.Models.Add(m)
                                            | TeXModel (tex,f,b,c,s) when tex.Contains(_t) -> 
                                                let (ExprTree.Assignment (n,e)) = f                                                
                                                let m = Model2.createEmpty ChartType.Line 100 (SKColor(uint32 c)) s
                                                evalModel2 maps _t b m
                                                m.name <- n
                                                c2.Models.Add(m)
                                                let arg = (tex,i,b)
                                                skchart.Args.Add(arg) |> ignore
                                                i <- i + 1
                                            | _ -> ()
                                        c2.XImg <- Converter.image _t
                                        c2.Update()   
                                        let tex_strs_subset = tex_strs |> Seq.filter (function tex -> tex.Contains(t))
                                        let vars = vars |> Array.filter (fun (s,v) -> s <> t && s <> tin.Current)
                                        let vars_subset = [
                                            for tex in tex_strs_subset do
                                                for (s0,v0) in vars do
                                                    if tex.Contains(s0) then yield (s0,v0)
                                        ]
                                        tout.Set t
                                        vars_subset |> vars_list.Set                                     
                                        tex_strs_subset |> List.ofSeq |> tex_fns.Set

                        )
                    ]
                    Border.create [Border.height 20; Border.width 200]                    
                ]
            ] :> IView
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
                    StackPanel.create [
                        StackPanel.dock Dock.Left
                        StackPanel.margin (Thickness(10.0, 0.0, 0.0, 0.0))
                        StackPanel.verticalScrollBarVisibility ScrollBarVisibility.Auto
                        StackPanel.horizontalScrollBarVisibility ScrollBarVisibility.Auto
                        StackPanel.children [

                            ToggleButton.create [
                                ToggleButton.content "notation"
                                ToggleButton.margin (Thickness(0., 20., 15., 20.))
                                ToggleButton.isChecked notation.Current
                                ToggleButton.onClick (fun _ -> notation.Set (not (notation.Current)))
                            ]
                            Button.create [
                                Button.content "capture"
                                Button.onClick (fun _ ->
                                    if not (System.IO.Directory.Exists("vertices")) then 
                                        System.IO.Directory.CreateDirectory("vertices") |> ignore
                                        
                                    let name = System.Guid.NewGuid().ToString()
                                    let path = System.IO.Path.Combine("vertices", name)
                                    use fs = System.IO.File.CreateText(path)
                                    if skchart.IsSKChart2 then
                                        let c2 = skchart.AsSKChart2()
                                        for m in c2.Models do
                                            fs.WriteLine "\n"
                                            for i in 0..m.xvalues.Length - 1 do
                                                fs.WriteLine($"{m.xvalues[i]}  {m.yvalues[i]}")
                                    if skchart.IsSKChart3 then
                                        let c3 = skchart.AsSKChart3()
                                        for m in c3.Models do
                                            fs.WriteLine "\n"
                                            for i in 0..m.xvalues.Length - 1 do
                                                fs.WriteLine($"{m.xvalues[i]}  {m.yvalues[i]}  {m.zvalues[i]}")
                                            
                                )
                            ]
                            
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

                            // ComboBoxComponent("select target-x", maps, _vars, target_x, target_y, models, vars_list, tex_fns)
                            // ComboBoxComponent("select target-y", maps, _vars, target_y, target_x, models, vars_list, tex_fns)
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
                                            let mutable i = 0
                                            for model in models do
                                                match model with
                                                | RawModel3 (_tx,_ty,_,m) -> if _tx = tx && ty = _ty then c3.Models.Add(m)
                                                | TeXModel (tex,f,b,c,s) when tex.Contains(tx) && tex.Contains(ty) ->
                                                    let m = Model3.createEmpty ChartType.Points 40 40 (SKColor(uint32 c)) s
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
                                            let mutable i = 0
                                            for model in models do
                                                match model with
                                                | RawModel3 (_tx,_ty,_,m) -> if _tx = tx && ty = _ty then c3.Models.Add(m)
                                                | TeXModel (tex,f,b,c,s) when tex.Contains(tx) && tex.Contains(ty) ->
                                                    let m = Model3.createEmpty ChartType.Points 40 40 (SKColor(uint32 c)) s
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

        
