#r "../bin/Release/net8.0/mkxkFS.dll"
#r "../bin/Release/net8.0/mkxk-viewer.dll"
#r "../bin/Release/net8.0/SKCharts.dll"
#r "../bin/Release/net8.0/SKCharts.Avalonia.dll"

#r "nuget: System.IO.Ports, 9.0.1"
#r "nuget: Avalonia, 11.2.3"
#r "nuget: Avalonia.Desktop, 11.2.3"
#r "nuget: Avalonia.Themes.Fluent, 11.2.3"
#r "nuget: Avalonia.FuncUI, 1.5.1"
#r "nuget: Avalonia.Fonts.Inter, 11.2.3"

open System
open System.IO.Ports
open MKXK
open ExprTree
open MKXK.Viewer
open SkiaSharp
// open SKCharts.Avalonia

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

open SKCharts



let [<Literal>] fstr = @"f(x) = 20"

let symbols: Symbols = {
    constants = []
    variables = []
    functions = []
}

let maps: Maps = {
    constants = Map[]
    variables = Map[]
    functions = Map[]
}

let f = Parser.parse symbols fstr
let b = Binder.bind f

let m = Model2.createEmpty ChartType.Line 30 SKColors.Blue 3.2f

let skchart = new SKChart2([("f(t)",m)])
skchart.ResetBounds()
skchart.Update()

// for s in SerialPort.GetPortNames() do printfn "%s" s
type Timer() =
    let mutable sec = 0
    let mutable min = 0
    let mutable h = 0
    let mutable d = 0

    member this.Inscrease(dt:int) =
        sec <- sec + dt / 1000
        if sec >= 60 then
            min <- min + 1
            sec <- sec - 60
        if min >= 60 then
            h <- h + 1
            min <- 0
        if h >= 24 then
            d <- d + 1
            h <- 0

    member this.Sec with get() = sec
    member this.Min with get() = min
    member this.H with get() = h
    member this.D with get() = d


let view() = Component(fun ctx ->
    let messages = ResizeArray<string>()
    let messages_state = ctx.useState messages
    let port_names = SerialPort.GetPortNames()
    let timer = Timer()
    let mutable capacity = 0
    let mutable port: SerialPort = null
    port <- new SerialPort(
        BaudRate = 115200,
        PortName = "/dev/ttyACM0",
        Parity = Parity.None,
        DataBits = 8,
        StopBits = StopBits.One
    )
    port.DataReceived.Add (fun evArgs -> 
        if evArgs <> null then
            let indata = port.ReadLine()

            timer.Inscrease(1000)
            let dt = 25. + 5. * Random.Shared.NextDouble() - 2.5
            let str = $"{timer.Sec}:{timer.Min}:{timer.H}:{timer.D}, temp:{dt}"
            let tidx = str.LastIndexOf(':') + 1
            let t = Double.Parse(str[tidx..])
            if capacity < m.yvalues.Length then
                m.xvalues[capacity] <- float (timer.Sec + timer.Min * 60)
                m.yvalues[capacity] <- t
                for i = capacity to m.yvalues.Length - 1 do
                    m.xvalues[i] <- m.xvalues[capacity]
                    m.yvalues[i] <- m.yvalues[capacity]
                capacity <- capacity + 1
            else 
                for i = 0 to capacity - 2 do
                    m.xvalues[i] <- m.xvalues[i + 1]
                    m.yvalues[i] <- m.yvalues[i + 1]
                m.xvalues[capacity - 1] <- float (timer.Sec + timer.Min * 60)
                m.yvalues[capacity - 1] <- t
            skchart.UpdateCachedBoundsY()

            // messages.Add indata
            messages.Add str
            messages_state.Set messages            
    )                
    
    
    DockPanel.create [
        DockPanel.lastChildFill true
        DockPanel.children [
            StackPanel.create [
                StackPanel.dock Dock.Left
                StackPanel.children [
                    ComboBox.create [
                        ComboBox.width 120
                        ComboBox.dataItems port_names
                        ComboBox.onSelectedItemChanged (fun arg ->
                            if arg <> null then
                                let s = arg :?> string
                                port.PortName <- s

                        )
                    ]
                    Button.create [
                        Button.content "open port"
                        Button.onClick (fun f ->
                            try
                                port.Open()
                            with 
                                | :? System.Exception as e -> printfn "%s" e.Message
                        )
                    ]
                    Button.create [
                        Button.content "close port"
                        Button.onClick (fun f ->
                            try
                                port.Close()
                            with 
                                | :? System.Exception as e -> printfn "%s" e.Message
                        )
                    ]                    
                ]
            ]
            ListBox.create [
                ListBox.dock Dock.Bottom
                ListBox.maxHeight 250
                ListBox.verticalScrollBarVisibility ScrollBarVisibility.Auto
                ListBox.dataItems messages_state.Current
            ]
            View.createGeneric<SKCharts.Avalonia.SKChart2Control> []
            |> View.withConstructorArgs [|skchart :> obj|]            
        ]
    ]

)

let renderer = Renderer("Serial port")
renderer.RunParallel(fun _ -> view())
Console.ReadKey()
