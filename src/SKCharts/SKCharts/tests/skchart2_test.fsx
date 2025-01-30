#r "../bin/Debug/net8.0/SKCharts.dll"
#r "nuget: Avalonia, 11.0.6"
#r "nuget: Avalonia.Desktop, 11.0.6"
#r "nuget: Avalonia.Themes.Fluent, 11.0.6"
#r "nuget: Avalonia.FuncUI, 1.1.0"
#r "nuget: Avalonia.Fonts.Inter, 11.0.6"

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

open System
open SkiaSharp
open SKCharts
open System.Numerics
open System.Linq
open SkiaSharp
open Avalonia
open Avalonia.Skia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform
open Avalonia.Threading
open Avalonia.Skia
open Avalonia.Interactivity
open Avalonia.Rendering.SceneGraph
open Avalonia.LogicalTree
open Avalonia.VisualTree


type private CustomDrawOp2(bounds:Rect, noSkia:GlyphRun, skchart:SKChart2) =
    let noSkia = noSkia.TryCreateImmutableGlyphRunReference()

    interface ICustomDrawOperation with
        member this.Bounds with get() = bounds

        member this.HitTest(p:Point) = false

        member this.Equals(other:ICustomDrawOperation) = false

        member this.Render(context:ImmediateDrawingContext) =
            let lease_feauture = context.TryGetFeature<ISkiaSharpApiLeaseFeature>()
            if lease_feauture = null then
                context.DrawGlyphRun(Brushes.Black, noSkia)
            else
                use lease = lease_feauture.Lease()
                let canvas = lease.SkCanvas
                canvas.Save() |> ignore
                skchart.Draw(canvas)                
                canvas.Restore()

        member this.Dispose() = ()

        

type SKChart2Control(skchart:SKChart2) =
    inherit Control()

    let text = "Current renderering API is not Skia"
    let glyphs = text.Select(fun ch -> Typeface.Default.GlyphTypeface.GetGlyph(uint32 ch)).ToArray() 
    let _noSkia = new GlyphRun(Typeface.Default.GlyphTypeface, 12, text.AsMemory(), glyphs)
    
    let mutable is_disposed = false

    interface IDisposable with
        member this.Dispose() = 
            if not is_disposed then
                (skchart :> IDisposable).Dispose()
            is_disposed <- true

    override this.Render(context:DrawingContext) =
        context.Custom(new CustomDrawOp2(Rect(0,0, this.Bounds.Width, this.Bounds.Height), _noSkia, skchart));
        Dispatcher.UIThread.InvokeAsync(this.InvalidateVisual, DispatcherPriority.Background) |> ignore

let viewTest() =
    Component(fun ctx -> 
        let tex = ctx.useState "f(x)"
        let textbox = ctx.useState<TextBox> null
        let x = [|for i in 0..100 -> float i|]
        let y = [|for i in 0..100 -> float i|]
        let m = Model2.create ChartType.Line x y (SKColors.Blue) 1.8f
        let c = new SKChart2([])
        c.AddModel m
        c.Update()
        let slider_val = ctx.useState 0.
        let r = Random()
        DockPanel.create [
            DockPanel.lastChildFill true
            DockPanel.children [
                StackPanel.create [
                    StackPanel.horizontalAlignment HorizontalAlignment.Center
                    StackPanel.dock Dock.Left
                    StackPanel.margin (Thickness(0., 0., 0., 15.))
                    StackPanel.children [
                        TextBlock.create [
                            TextBlock.text (slider_val.Current.ToString("N3"))
                        ]
                        Slider.create [
                            Slider.width 100
                            Slider.minimum 0
                            Slider.maximum 100
                            Slider.onValueChanged (fun d -> 
                                for i in 0..y.Length - 1 do
                                    let dy = 100. * r.NextDouble() - 50.
                                    m.yvalues[i] <- m.yvalues[i] + dy
                                c.Update()

                                // slider_val.Set d
                            )
                        ]
                    ]
                ]
                ContentControl.create [
                    ContentControl.content (new SKChart2Control(c)) 
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
