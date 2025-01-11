#r "../bin/Debug/net8.0/Notation.dll"
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
open Avalonia.FuncUI.DSL
open Avalonia.Layout

module Converter =
    open Notation
    open System.Linq

    let ms = new System.IO.MemoryStream(8 * 1024)

    let convert (transparent:bool) (tex:string) = 
        ms.Position <- 0
        try
            let exprs = Parser.parseExprs tex
            Typesetting.render transparent ms exprs           
            ms.Position <- 0
            new Avalonia.Media.Imaging.Bitmap(ms)
        with 
            | _ -> 
                Typesetting.errorImg ms
                ms.Position <- 0
                new Avalonia.Media.Imaging.Bitmap(ms)


let latexContent (str:string) =
    if str.Contains '$' then
        let a = str.IndexOf '$'
        let b = str.LastIndexOf '$'
        str[a..b - 1]
    else str


let viewTest() =
    Component(fun ctx -> 
        let tex = ctx.useState "f(x)"
        let textbox = ctx.useState<TextBox> null
        DockPanel.create [
            DockPanel.lastChildFill true
            DockPanel.children [
                StackPanel.create [
                    StackPanel.horizontalAlignment HorizontalAlignment.Center
                    StackPanel.dock Dock.Bottom
                    StackPanel.margin (Thickness(0., 0., 0., 15.))
                    StackPanel.children [

                        Button.create [
                            Button.content "convert"
                            Button.onClick (fun _ -> tex.Set textbox.Current.Text)
                        ]
                        TextBox.create [
                            TextBox.init textbox.Set
                            TextBox.height 300
                            TextBox.width 800                        
                        ]
                        
                    ]
                ]
                Image.create [
                    Image.stretch Media.Stretch.None
                    Image.source (Converter.convert true (latexContent tex.Current))
                ]
            ]
        ]    
    )

type MainWindow() =
    inherit HostWindow()
    do
        base.Title <- "NotationFS"
        base.Content <- viewTest()
    

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add (FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Light

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as dsk -> dsk.MainWindow <- MainWindow()
        | _ -> ()


let buildAvaloniaApp () = 
    AppBuilder
        .Configure<App>()
        .UsePlatformDetect()
        .WithInterFont()
        .LogToTrace(areas = Array.empty)
        
buildAvaloniaApp().StartWithClassicDesktopLifetime([||])
