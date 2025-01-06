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


type App<'T when 'T :> Component and 'T : (new : unit -> 'T)>() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Light

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop -> 
            desktop.MainWindow <- new Window()
            desktop.MainWindow.Title <- "MKXK"
            desktop.MainWindow.Content <- new 'T()
        | _ -> ()


type Renderer<'T when 'T :> Component and 'T : (new : unit -> 'T)>() =
    member x.Run() =
        ignore <| AppBuilder
            .Configure<App<'T>>()
            .UsePlatformDetect()
            .WithInterFont()
            .LogToTrace(areas = Array.empty)
            .StartWithClassicDesktopLifetime([||])


type Renderer() =
    let mutable wnd: Window = null
    let mutable thread: System.Threading.Thread = null    


    /// componentfn: function that returns a Component to be used as content of the MainWindow
    member x.Run(componentfn: unit -> Component) =
        ignore <| AppBuilder
            .Configure<Views.App>()
            .UsePlatformDetect()
            .WithInterFont()
            .LogToTrace(areas = Array.empty)
            .StartWithClassicDesktopLifetime([||])

        let rec set_window () =
            System.Threading.Thread.Sleep 300; printfn "sleep 300"
            if Application.Current <> null then
                match Application.Current.ApplicationLifetime with
                | :? IClassicDesktopStyleApplicationLifetime as desktop -> 
                    Avalonia.Threading.Dispatcher.UIThread.Invoke (fun _ -> desktop.MainWindow.Content <- componentfn ())                
                | _ -> ()
            else 
                set_window ()

        set_window ()
            

        
     
    /// componentfn: function that returns a Component to be used as content of the MainWindow
    /// this functions executes in a different thread, letting the main thread non-blocking. 
    member x.RunParallel(componentfn: unit -> Component) =
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
                
        Avalonia.Threading.Dispatcher.UIThread.Invoke (fun _ -> wnd.Content <- componentfn ())


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
