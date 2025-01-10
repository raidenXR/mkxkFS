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


// type private CustomDrawOp2(bounds:Rect, noSkia:GlyphRun, skchart:SKChart2) =
//     let noSkia = noSkia.TryCreateImmutableGlyphRunReference()

//     interface ICustomDrawOperation with
//         member this.Bounds with get() = bounds

//         member this.HitTest(p:Point) = false

//         member this.Equals(other:ICustomDrawOperation) = false

//         member this.Render(context:ImmediateDrawingContext) =
//             let lease_feauture = context.TryGetFeature<ISkiaSharpApiLeaseFeature>()
//             if lease_feauture = null then
//                 context.DrawGlyphRun(Brushes.Black, noSkia)
//             else
//                 use lease = lease_feauture.Lease()
//                 let canvas = lease.SkCanvas
//                 canvas.Save() |> ignore
//                 skchart.Draw(canvas)                
//                 canvas.Restore()

//         member this.Dispose() = ()       


// type private CustomDrawOp3(bounds:Rect, noSkia:GlyphRun, skchart:SKChart3) =
//     let noSkia = noSkia.TryCreateImmutableGlyphRunReference()

//     interface ICustomDrawOperation with
//         member this.Bounds with get() = bounds

//         member this.HitTest(p:Point) = false

//         member this.Equals(other:ICustomDrawOperation) = false

//         member this.Render(context:ImmediateDrawingContext) =
//             let lease_feauture = context.TryGetFeature<ISkiaSharpApiLeaseFeature>()
//             if lease_feauture = null then
//                 context.DrawGlyphRun(Brushes.Black, noSkia)
//             else
//                 use lease = lease_feauture.Lease()
//                 let canvas = lease.SkCanvas
//                 canvas.Save() |> ignore
//                 skchart.Draw(canvas)                
//                 canvas.Restore()

//         member this.Dispose() = ()


type private CustomDrawOp(bounds:Rect, noSkia:string, skchart:SKChart) =
    // let noSkia = noSkia.TryCreateImmutableGlyphRunReference()

    interface ICustomDrawOperation with
        member this.Bounds with get() = bounds

        member this.HitTest(p:Point) = false

        member this.Equals(other:ICustomDrawOperation) = false

        member this.Render(context:ImmediateDrawingContext) =
            let lease_feauture = context.TryGetFeature<ISkiaSharpApiLeaseFeature>()
            if lease_feauture = null then
                // context.DrawGlyphRun(Brushes.Black, noSkia)
                failwith noSkia
            else
                use lease = lease_feauture.Lease()
                let canvas = lease.SkCanvas
                canvas.Save() |> ignore
                skchart.Draw(canvas)                
                canvas.Restore()

        member this.Dispose() = ()       
        
// type SKChart2Control(skchart:SKChart2) =
//     inherit Control()

//     let text = "Current renderering API is not Skia"
//     let glyphs = text.Select(fun ch -> Typeface.Default.GlyphTypeface.GetGlyph(uint32 ch)).ToArray() 
//     let _noSkia = new GlyphRun(Typeface.Default.GlyphTypeface, 12, text.AsMemory(), glyphs)
    
//     let mutable is_disposed = false

//     interface IDisposable with
//         member this.Dispose() = 
//             if not is_disposed then
//                 (skchart :> IDisposable).Dispose()
//             is_disposed <- true

//     override this.Render(context:DrawingContext) =
//         context.Custom(new CustomDrawOp2(Rect(0,0, this.Bounds.Width, this.Bounds.Height), _noSkia, skchart));
//         Dispatcher.UIThread.InvokeAsync(this.InvalidateVisual, DispatcherPriority.Background) |> ignore


// type SKChart3Control(skchart:SKChart3) =
//     inherit Control()

//     let text = "Current renderering API is not Skia"
//     let glyphs = text.Select(fun ch -> Typeface.Default.GlyphTypeface.GetGlyph(uint32 ch)).ToArray() 
//     let _noSkia = new GlyphRun(Typeface.Default.GlyphTypeface, 12, text.AsMemory(), glyphs)
    
//     let mutable is_disposed = false

//     interface IDisposable with
//         member this.Dispose() = 
//             if not is_disposed then
//                 (skchart :> IDisposable).Dispose()
//             is_disposed <- true

//     override this.Render(context:DrawingContext) =
//         context.Custom(new CustomDrawOp3(Rect(0,0, this.Bounds.Width, this.Bounds.Height), _noSkia, skchart));
//         Dispatcher.UIThread.InvokeAsync(this.InvalidateVisual, DispatcherPriority.Background) |> ignore



/// encapsulates SKChart2 & SKChart3 as strategy pattern
type SKChartControl(skchart:SKChart) =
    inherit Control()

    let mutable skchart = skchart
    let mutable count = 0
    // let mutable skchart: SKChart = new SKChart(new SKChart2([]), new SKChart3([],Colormap.Hot))

    let text = "Current renderering API is not Skia"
    // let glyphs = text.Select(fun ch -> Typeface.Default.GlyphTypeface.GetGlyph(uint32 ch)).ToArray() 
    // let _noSkia = new GlyphRun(Typeface.Default.GlyphTypeface, 12, text.AsMemory(), glyphs)
    let _noSkia = text

    // do
    //     let c2 = skchart.AsSKChart2()
    //     c2.Background <- SKColors.Orange
    //     let c3 = skchart.AsSKChart3()
    //     c3.Background <- SKColors.Gold

    // interface IDisposable with
    //     member this.Dispose() =
    //         if skchart <> null then (skchart :> IDisposable).Dispose()

    new() = new SKChartControl(new SKChart(new SKChart2([]), new SKChart3([], Colormap.Hot)))

            

    override this.Render(context:DrawingContext) =
        if skchart <> null then
            count <- count + 1
            let w = skchart.W
            let h = skchart.H
            let width = this.Width
            let height = this.Height         
            
            
            // if skchart.IsSKChart2 then
                // let c2 = skchart.AsSKChart2()
                // printfn "with models_count: %d bounds: %A" (c2.Models.Count) (c2.Bounds)
                // printfn "skchart2 before draw context"
                // context.Custom(new CustomDrawOp2(Rect(0,0, this.Bounds.Width, this.Bounds.Height), _noSkia, skchart.AsSKChart2()));
                // printfn "skchart2 before invalidate"
                // Dispatcher.UIThread.InvokeAsync(this.InvalidateVisual, DispatcherPriority.Background) |> ignore
            // elif skchart.IsSKChart3 then
                // let c3 = skchart.AsSKChart3()
                // printfn "with models_count: %d bounds: %A" (c3.Models.Count) (c3.Bounds)
                // printfn "skchart3 before draw context"
                // context.Custom(new CustomDrawOp3(Rect(0,0, this.Bounds.Width, this.Bounds.Height), _noSkia, skchart.AsSKChart3()));
                // printfn "skchart3 before invalidate"
                // Dispatcher.UIThread.InvokeAsync(this.InvalidateVisual, DispatcherPriority.Background) |> ignore
            
            context.Custom(new CustomDrawOp(Rect(0,0, this.Bounds.Width, this.Bounds.Height), _noSkia, skchart));
            // printfn "skchart drawing count: %d, width: %g, height: %g, w: %g, h: %g" count width height w h 
        else
            printfn "skchart is null"

        // You invalidate visual after each Render call. Basically, creating a control that requests a new render on each frame
        // Dispatcher.UIThread.InvokeAsync(this.InvalidateVisual, DispatcherPriority.Background) |> ignore


    member this.SKChart 
        with get() = skchart
        and set(value) = 
            skchart <- value
            printfn "no_of models: %d" (if skchart.IsSKChart2 then skchart.C2.Models.Count else skchart.C3.Models.Count)
