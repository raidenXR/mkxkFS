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
        


module Model2 =
    
    type TeXModel = | TeXModel of string * ExprTree.Expr * Binder.BoundExpr * SKCharts.Model2D

    
    let createpoints x y (c:Colors) s =
        let m = SKCharts.Model2D.CreatePoints(x, y, SKColor(uint c))
        m.Paint.StrokeWidth <- s
        m

    let createline x y (c:Colors) s =
        let m = SKCharts.Model2D.CreateLine(x, y, SKColor(uint c))
        m.Paint.StrokeWidth <- s
        m

    let createTeXModel (cons:Map<string,float>) (vars:Map<string,Variable>) (fns:Map<string,Binder.BoundExpr>) tex t (x:array<float>) c s =
        let f = Parser.parse tex cons.Keys vars.Keys fns.Keys
        let b = Binder.bind f
        let y = Array.zeroCreate<float> x.Length
        Evaluation.evalvalues x y cons vars fns t b
        let m = createline x y c s
        (tex, f, b, m)





module Views =
    open Avalonia.FuncUI.Builder
    open Avalonia.FuncUI.Types

    type ToggleButton with
        static member content<'t when 't :> ToggleButton>(value:obj) :IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<obj>(ToggleButton.ContentProperty, value, ValueNone)


    let view2 (constants:Map<string,float>, variables:Map<string,Variable>, functions:Map<string,Binder.BoundExpr>) =
        Component(fun ctx ->
            let tex = ctx.useState ""  // constants
            let cons = [|for x in constants.Keys -> x,constants[x]|] |> ctx.useState
            let vars = [|for x in variables.Keys -> x,variables[x]|] |> ctx.useState
            let fns  = [|for x in functions.Keys -> x,functions[x]|] |> ctx.useState
            let c = new SKChart2DControl()
            let m = Model2.createpoints [|0..100|] [|0..100|] Colors.SteelBlue 4.8f
            c.AttachModel m
        
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
                                ListBox.dataItems cons.Current
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
                                ListBox.dataItems vars.Current
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
                                ListBox.dataItems fns.Current
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
                                ComboBox.dataItems vars.Current
                                ComboBox.itemTemplate (
                                    DataTemplateView<string * Variable>.create (fun (s,v) ->
                                        Image.create [
                                            Image.stretch Media.Stretch.None 
                                            Image.source (Converter.convert s)
                                        ]        
                                    )
                                )
                            ]
                            
                            Border.create [Border.height 20; Border.width 200]
                            Button.create [
                                Button.content "capture_vert"
                                Button.onClick (fun _ -> ())
                            ]                            
                        ]
                    ]

                    Border.create [Border.dock Dock.Top; Border.height 20; Border.width 200]
                    TextBlock.create []
                    Image.create [
                        // Image.source (Converter.convert (Seq.last ))
                    ]

                    ListBox.create [
                        ListBox.maxHeight 200
                        ListBox.margin (Thickness(0.0, 0.0, 0.0, 10.0))
                        ListBox.dock Dock.Bottom
                        ListBox.verticalScrollBarVisibility ScrollBarVisibility.Auto
                        ListBox.horizontalScrollBarVisibility ScrollBarVisibility.Auto
                        ListBox.dataItems vars.Current
                        ListBox.itemTemplate (
                            DataTemplateView<string * Variable>.create (fun (s, v) -> 
                                // let slider = ctx.useState<Slider>(null)
                                Grid.create [
                                    Grid.columnDefinitions "100, 140, 200"
                                    Grid.children [
                                        Image.create [
                                            Image.column 0
                                            Image.stretch Media.Stretch.None 
                                            Image.horizontalAlignment HorizontalAlignment.Left
                                            Image.source (Converter.convert s)
                                        ]
                                        Slider.create [
                                            Slider.column 1
                                            Slider.width 100
                                            Slider.minimum v.A
                                            Slider.maximum v.B    
                                            // Slider.onValueChanged (fun (s,v) - >)
                                        ]
                                        TextBlock.create [
                                            TextBlock.column 2
                                            TextBlock.width 100
                                            TextBlock.maxWidth 100
                                            // TextBlock.text (string slider.Current.Value)
                                        ]
                                    ]
                                ]
                            )
                        )
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

        
