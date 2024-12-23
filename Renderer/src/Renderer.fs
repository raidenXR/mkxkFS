namespace RendererFS

open System
open MKXK
open SkiaSharp
open Avalonia

open SKCharts

open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout


type Renderer(maps:Maps, models:list<Model2.Model>) =
    let mutable wnd: Window = null
    let mutable thread: System.Threading.Thread = null    
     
    member x.Run() =
        let app () = 
            ignore <| AppBuilder.Configure<Views.App>().UsePlatformDetect().UseSkia().StartWithClassicDesktopLifetime([||])

        thread <- System.Threading.Thread(app)
        thread.Start()       

        let mutable n = 0
        let mutable app = null
        let mutable msg = "window initializing"
        
        while wnd = null do
            msg <- msg + "."
            Console.WriteLine msg
            let struct(i, j) = Console.GetCursorPosition()
            Console.SetCursorPosition(0, j - 1)
            System.Threading.Thread.Sleep(300)
            if app = null then app <- Application.Current
            if app <> null then
                match app.ApplicationLifetime with
                | :? IClassicDesktopStyleApplicationLifetime as desktop -> wnd <- desktop.MainWindow
                | _ -> ()
                
        Avalonia.Threading.Dispatcher.UIThread.Invoke (fun _ -> wnd.Content <- Views.view2(maps, models))


    member x.Close() =
        wnd.Close()

    new(maps:Maps, models:list<string*Model2.Model>) =
        let (names,models) = List.unzip models
        Model2.setNames names models
        Renderer(maps,models)


type GenericRenderer() =
    let mutable wnd: Window = null
    let mutable thread: System.Threading.Thread = null

    member x.Run() =
        let app () = 
            ignore <| AppBuilder.Configure<Views.App>().UsePlatformDetect().UseSkia().StartWithClassicDesktopLifetime([||])

        thread <- System.Threading.Thread(app)
        thread.Start()

        let mutable n = 0
        let mutable app = null
        let mutable msg = "window initializing"
        
        while wnd = null do
            msg <- msg + "."
            Console.WriteLine msg
            let struct(i, j) = Console.GetCursorPosition()
            Console.SetCursorPosition(0, j - 1)
            System.Threading.Thread.Sleep(300)
            if app = null then app <- Application.Current
            if app <> null then
                match app.ApplicationLifetime with
                | :? IClassicDesktopStyleApplicationLifetime as desktop -> wnd <- desktop.MainWindow
                | _ -> ()
                
        Avalonia.Threading.Dispatcher.UIThread.Invoke (fun _ -> wnd.Content <- Views.viewTest())

    member x.Close() =
        wnd.Close()

        

module Program =
    [<CompiledName "BuildAvaloniaApp">] 
    let buildAvaloniaApp () = 
        AppBuilder
            .Configure<Views.App>()
            .UsePlatformDetect()
            .WithInterFont()
            .LogToTrace(areas = Array.empty)

    [<EntryPoint; STAThread>]
    let main argv =
        buildAvaloniaApp().StartWithClassicDesktopLifetime(argv)
