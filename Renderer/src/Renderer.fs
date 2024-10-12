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


type Renderer(constants:Map<string,float>, variables:Map<string,Variable>, functions:Map<string,Binder.BoundExpr>, nsamples:int) =
    let mutable wnd: Window = null
    let mutable thread: System.Threading.Thread = null
    let mutable chart = new SKCharts.SKChart2D()
    
    let tex_models = System.Collections.Generic.List<Model2.TeXModel>()
    
    // member x.Run() =
    //     thread <- Thread.Start(...)
        
    
    // member x.Close() = ...
    //     chart.Dispose()
    
    
    
    // hold multiple latex models
    let update_slider (arg:string * float) = 
        let (t, v) = arg
        variables[t].V <- ValueSome v
    
        for (Model2.TeXModel (tex,f,b,m)) in tex_models do
            Evaluation.evalvalues m.Xvalues m.Yvalues constants variables functions t b
            m.UpdateBounds()
        
        chart.UpdateBounds()
        chart.Update() 
            
            
    let update_combobox (arg:string) =
        let t = arg
        let a = variables[arg].A
        let b = variables[arg].B
        let dx = (b - a) / (float nsamples)
                     
        for Model2.TeXModel (tex,f,b,m) in tex_models do
            for i in 0..m.Xvalues.Length - 1 do
                m.Xvalues[i] <- dx * (float i)
            Evaluation.evalvalues m.Xvalues m.Yvalues constants variables functions t b
            m.UpdateBounds()
        
        chart.UpdateBounds()
        chart.Update() 
    
    
    // member x.Latex(x0, x1, t, tex) =
    // 	let f = Parser(tex).parse()
    //     let vars = variables f
    //     let cons = constants f
    //     let fns = functions f
        
        // evaluate latex
        // evalxvalues x y cons vars fns t (Binder.bind f)
        
        // update model - chart
        // model_updateXY
        // chart_updateXY atIdx 0 x y
        
        
        // call from main thread
        // let updateMainThread () = 
        
        
        // call only from renderer thread
        // let updateUIthread () =
        //     wnd.LatexBlock.Text <- tex
        //     wnd.ViewModel.Latex < tex
        //     wnd.VariablesList <- ObservableCollection<VariableInfo>()
        //     wnd.ConstantsList <- ObservableCollection<Constant>()
        //     wnd.FunctionsList <- ObservableCollection<string>()            
           
        
        // if wnd <> null then Dispatcer.UIThread.Invoke(updateUIThread)
        
   
    member x.Chart with get() = chart      
    
    member x.RemoveModelAt idx = chart.Models.RemoveAt idx
    
    member x.AddModel (model:obj) =
        match model with
        | :? Model2D as m -> chart.Models.Add m
        | :? Model2.TeXModel as texm -> 
            tex_models.Add texm
            let (Model2.TeXModel (tex,f,b,m)) = texm
            chart.Models.Add m
        | _ -> failwith "improper type of model"
    
     
    member x.Run() =
        let app () = 
            ignore <| AppBuilder.Configure<Views.App>().UsePlatformDetect().UseSkia().StartWithClassicDesktopLifetime([||])

        thread <- System.Threading.Thread(app)
        thread.Start()       

        // let rec loop (n:int) =
        //     Console.WriteLine("window initializing".PadRight(n, '.'))
        //     let struct(i, j) = Console.GetCursorPosition()
        //     Console.SetCursorPosition(0, j - 1)
        //     if Application.Current <> null then
        //         match Application.Current.ApplicationLifetime with            
        //         | :? IClassicDesktopStyleApplicationLifetime as desktop -> 
        //             wnd <- desktop.MainWindow
        //             // wnd.Content <- Views.view2(constants, variables, functions)
        //             Avalonia.Threading.Dispatcher.UIThread.Invoke (fun _ -> wnd.Content <- Views.view2(constants, variables, functions))
        //         | _ -> System.Threading.Thread.Sleep(300); loop (n + 1)
        //     else System.Threading.Thread.Sleep(300); loop (n + 1)
        // loop (0)    
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
                
        Avalonia.Threading.Dispatcher.UIThread.Invoke (fun _ -> wnd.Content <- Views.view2(constants, variables, functions))



    member x.Close() =
        chart.Dispose()
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
