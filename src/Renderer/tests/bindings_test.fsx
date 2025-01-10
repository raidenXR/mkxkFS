#r "nuget: Avalonia, 11.2.3"
#r "nuget: Avalonia.Desktop, 11.2.3"
#r "nuget: Avalonia.Themes.Fluent, 11.2.3"
#r "nuget: Avalonia.FuncUI, 1.5.1"
#r "nuget: Avalonia.Fonts.Inter, 11.2.3"

open Avalonia
open Avalonia.Controls.Primitives
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.FuncUI
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Rendering
open Avalonia.VisualTree

open System


module Views =

    let LeftComponent (_series:list<string>, target_0:IWritable<string>, target_1:IReadable<string>, series0:IWritable<list<string>>, series1:IReadable<list<string>>) = 
        Component.create("left_component", fun ctx ->
            let target_0 = ctx.usePassed target_0
            let target_1 = ctx.usePassedRead target_1
            let series0 = ctx.usePassed series0
            let series1 = ctx.usePassedRead series1
    
            ctx.attrs [
                Component.dock Dock.Left
            ]           
        
            StackPanel.create [
                StackPanel.children [
                        ListBox.create [
                            ListBox.dataItems (List.concat [series0.Current; series1.Current] |> List.distinct)
                        ]
                        ComboBox.create [
                            ListBox.dataItems series1.Current
                            ComboBox.onSelectedItemChanged (fun sender -> 
                                let s = sender :?> string

                                target_0.Set s
                            
                                _series
                                |> List.except [s; target_1.Current]
                                |> series0.Set
                            )
                        ]
                
                ]
            ] :> IView
    )

    let RightComponent (_series:list<string>, target_0:IWritable<string>, target_1:IReadable<string>, series0:IWritable<list<string>>, series1:IReadable<list<string>>) = 
        Component.create("left_component", fun ctx ->
            let target_0 = ctx.usePassed target_0
            let target_1 = ctx.usePassedRead target_1
            let series0 = ctx.usePassed series0
            let series1 = ctx.usePassedRead series1
    
            ctx.attrs [
                Component.dock Dock.Right
            ]           
        
            StackPanel.create [
                StackPanel.children [
                        ListBox.create [
                            ListBox.dataItems series1.Current
                        ]
                        ComboBox.create [
                            ComboBox.dataItems _series
                            ComboBox.onSelectedItemChanged (fun sender -> 
                                let s = sender :?> string

                                target_0.Set s
                            
                                _series
                                |> List.except [s; target_1.Current]
                                |> series0.Set
                            )
                        ]
                
                ]
            ] :> IView
    )

    let mainView() = Component(fun ctx -> 
        let _series = [for i in 0..10 -> sprintf "v. %d" i]
        let series0 = ctx.useState _series
        let series1 = ctx.useState _series
        let series2 = ctx.useState _series
        let series3 = ctx.useState _series

        let target_0 = ctx.useState String.Empty
        let target_1 = ctx.useState String.Empty


        DockPanel.create [
            DockPanel.children [
                LeftComponent(_series, target_0, target_1, series0, series1)
                RightComponent(_series, target_1, target_0, series1, series0)
            ]
        ]
    )
        
    type MainWindow() =
        inherit HostWindow()
        do
            base.Title <- "MKXK"
            base.Content <- mainView()
        

    type App() =
        inherit Application()

        override this.Initialize() =
            this.Styles.Add (FluentTheme())
            this.RequestedThemeVariant <- Styling.ThemeVariant.Light

        override this.OnFrameworkInitializationCompleted() =
            match this.ApplicationLifetime with
            | :? IClassicDesktopStyleApplicationLifetime as dsk -> dsk.MainWindow <- MainWindow()
            | _ -> ()

            

ignore <| AppBuilder
    .Configure<Views.App>()
    .UsePlatformDetect()
    .UseSkia()
    .StartWithClassicDesktopLifetime([||])
