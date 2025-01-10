namespace RendererFS

open System
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

open SKCharts


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


type private CustomDrawOp(bounds:Rect, noSkia:GlyphRun, skchart:ISKChart) =
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



/// encapsulates SKChart2 & SKChart3 as strategy pattern
type SKChartControl() =
    inherit Control()

    let c2 = new SKChart2([])
    let c3 = new SKChart3([], Colormap.Hot)
    let tags = new System.Collections.Generic.List<string * int>()
    let args = new System.Collections.ArrayList()
    // let mutable c: ISKChart = c2
    let mutable is_c2 = true
    let mutable is_c3 = false
    let mutable is_disposed = false

    let text = "Current renderering API is not Skia"
    let glyphs = text.Select(fun ch -> Typeface.Default.GlyphTypeface.GetGlyph(uint32 ch)).ToArray() 
    let _noSkia = new GlyphRun(Typeface.Default.GlyphTypeface, 12, text.AsMemory(), glyphs)


    interface IDisposable with
        member this.Dispose() =
            if not is_disposed then
                (c2 :> IDisposable).Dispose()
                (c3 :> IDisposable).Dispose()
            is_disposed <- true
            

    override this.Render(context:DrawingContext) =
        // context.Custom(new CustomDrawOp(Rect(0,0, this.Bounds.Width, this.Bounds.Height), _noSkia, c));
        // Dispatcher.UIThread.InvokeAsync(this.InvalidateVisual, DispatcherPriority.Background) |> ignore
        if is_c2 then
            context.Custom(new CustomDrawOp2(Rect(0,0, this.Bounds.Width, this.Bounds.Height), _noSkia, c2));
            Dispatcher.UIThread.InvokeAsync(this.InvalidateVisual, DispatcherPriority.Background) |> ignore
        if is_c3 then
            context.Custom(new CustomDrawOp3(Rect(0,0, this.Bounds.Width, this.Bounds.Height), _noSkia, c3));
            Dispatcher.UIThread.InvokeAsync(this.InvalidateVisual, DispatcherPriority.Background) |> ignore


    member this.Tags with get() = tags

    member this.Args with get() = args

    member this.AsSKChart2() = 
        // c <- c2
        is_c2 <- true
        is_c3 <- false
        c2

    member this.AsSKChart3() =
        // c <- c3
        is_c3 <- true
        is_c2 <- false
        c3

    member this.IsSKChart2 
        with get() = is_c2
        and set(value) = 
            is_c2 <- value
            is_c3 <- not value
            // if value then c <- c2 else c <- c3
            
    member this.IsSKChart3 
        with get() = is_c3
        and set(value) = 
            is_c3 <- value
            is_c2 <- not value
            // if value then c <- c3 else c <- c2
