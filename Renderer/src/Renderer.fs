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


type Renderer(constants:Map<string,float>, variables:Map<string,Variable>, functions:Map<string,Binder.BoundExpr>, models:list<Model2.Model>) =
    let mutable wnd: Window = null
    let mutable thread: System.Threading.Thread = null
    // let mutable chart = new SKCharts.SKChart2D()  
    
    // hold multiple latex models
    // let update_slider (arg:string * float) = 
    //     let (t, v) = arg
    //     variables[t].V <- ValueSome v
    
    //     for model in models do
    //         match model with
    //         | Model2.TeXModel (tex, f, b, m) ->
    //             Evaluation.evalvalues m.Xvalues m.Yvalues constants variables functions t b
    //             m.UpdateBounds()
    //         | Model2.Model2D m -> ()                
        
    //     chart.UpdateBounds()
    //     chart.Update() 
            
            
    // let update_combobox (arg:string) =
    //     let t = arg
    //     let a = variables[arg].A
    //     let b = variables[arg].B
    //     let dx = (b - a) / (float nsamples)
                     
    //     for model in models do
    //         match model with
    //         | Model2.TeXModel (tex, f, b, m) ->
    //             for i in 0..m.Xvalues.Length - 1 do
    //                 m.Xvalues[i] <- dx * (float i)
    //             Evaluation.evalvalues m.Xvalues m.Yvalues constants variables functions t b
    //             m.UpdateBounds()
    //         | Model2.Model2D m -> ()
        
    //     chart.UpdateBounds()
    //     chart.Update()    
    
     
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
                
        Avalonia.Threading.Dispatcher.UIThread.Invoke (fun _ -> wnd.Content <- Views.view2(constants, variables, functions, models))



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
