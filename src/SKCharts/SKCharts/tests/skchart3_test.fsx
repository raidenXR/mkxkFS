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


type private CustomDrawOp3(bounds:Rect, noSkia:GlyphRun, skchart:SKChart3) =
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

        

type SKChart3Control(skchart:SKChart3) =
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
        context.Custom(new CustomDrawOp3(Rect(0,0, this.Bounds.Width, this.Bounds.Height), _noSkia, skchart));
        Dispatcher.UIThread.InvokeAsync(this.InvalidateVisual, DispatcherPriority.Background) |> ignore


let peak3d (w:outref<int>) (h:outref<int>) =
    let xmin = -3.
    let xmax = 3.
    let ymin = -3.
    let ymax = 3.
    let zmin = -8.
    let zmax = 8.

    let xlimitmin = xmin
    let ylimitmin = ymin
    let xspacing = 0.2
    let yspacing = 0.2
    let xnumber = int ((xmax - xmin) / xspacing) + 1
    let ynumber = int ((ymax - ymin) / yspacing) + 1
    let xpts = Array.zeroCreate<float> (xnumber * ynumber)
    let ypts = Array.zeroCreate<float> (xnumber * ynumber)
    let zpts = Array.zeroCreate<float> (xnumber * ynumber)
    w <- int xnumber
    h <- int ynumber

    for i in 0..xnumber - 1 do
        for j in 0..ynumber - 1 do
            let x = xlimitmin + xspacing * (float i)
            let y = ylimitmin + yspacing * (float j)
            let z = 3. * Math.Pow((1. - x), 2.) *
                    Math.Exp(-x * x - (y + 1.) * (y + 1.)) - 10. *
                    (0.2 * x - Math.Pow(x, 3.) - Math.Pow(y, 5.)) *
                    Math.Exp(-x * x - y * y) - 1. / 3. *
                    Math.Exp(-(x + 1.) * (x + 1.) - y * y)
            xpts[i * w + j] <- x
            ypts[i * w + j] <- y
            zpts[i * w + j] <- z
    (xpts,ypts,zpts)


let sin3d (w:outref<int>) (h:outref<int>) =
    let xmin = -3.
    let xmax = 3.
    let ymin = -3.
    let ymax = 3.
    let zmin = -8.
    let zmax = 8.
    let xtick = 4.
    let ytick = 4.
    let ztick = 0.5

    let xlimitmin = xmin
    let ylimitmin = ymin
    let xspacing = 0.2
    let yspacing = 0.2
    w <- int ((xmax - xmin) / xspacing) + 1
    h <- int ((ymax - ymin) / yspacing) + 1
    let xpts = Array.zeroCreate<float> (w * h)
    let ypts = Array.zeroCreate<float> (w * h)
    let zpts = Array.zeroCreate<float> (w * h)
    for i = 0 to w - 1 do
        for j = 0 to h - 1 do
            let x = xlimitmin + xspacing * (float i)
            let y = ylimitmin + yspacing * (float j)
            let r = sqrt (x * x + y * y) + 0.000001
            let z = sin (r) / r
            xpts[i * w + j] <- x
            ypts[i * w + j] <- y
            zpts[i * w + j] <- z
    (xpts,ypts,zpts)
    


let viewTest() =
    Component(fun ctx -> 
        let tex = ctx.useState "f(x)"
        let textbox = ctx.useState<TextBox> null
        let mutable w = 0
        let mutable h = 0
        let (x,y,z) = peak3d &w &h
        let m0 = Model3.create ChartType.Surface x y z w h (SKColors.Blue) 2f
        let (x,y,z) = sin3d &w &h
        let m1 = Model3.create ChartType.Surface x y z w h (SKColors.Blue) 2f
        let c = new SKChart3([m0], Colormap.Gray)
        let slider_val0 = ctx.useState 0.
        let slider_val1 = ctx.useState 0.
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
                            TextBlock.text (slider_val0.Current.ToString("N3"))
                        ]
                        Slider.create [
                            Slider.width 100
                            Slider.minimum -40
                            Slider.maximum 40
                            Slider.onValueChanged (fun d -> 
                                c.Camera.Azimuth <- float32 d
                                c.Update()
                                // slider_val0.Set d
                            )
                        ]
                        TextBlock.create [
                            TextBlock.text (slider_val1.Current.ToString("N3"))
                        ]
                        Slider.create [
                            Slider.width 100
                            Slider.minimum -40
                            Slider.maximum 40
                            Slider.onValueChanged (fun d ->
                                c.Camera.Elevation <- float32 d
                                c.Update()
                                // slider_val1.Set d
                            )
                        ]
                    ]
                ]
                ContentControl.create [
                    ContentControl.content (new SKChart3Control(c)) 
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
