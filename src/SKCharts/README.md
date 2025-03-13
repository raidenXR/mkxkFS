### SKCharts

Re-implemntation of an older .csproj to F#.     
Simple library for charting with SkiaSharp, designed to be lightweight enough for   
fast enough refresh rate.  
It can be used in any app that provides a graphics context for SkiaSharp and has   
a reference to some SKSurface  (and SKCanvas) object.    

On `tests/` directory it contains simple examples for both `SKChart2` and `SKChart3` classes, rendering on an AvaloniaUI window.  
Run examples with command  

```
dotnet fsi tests/skchart2_test.fsx
dotnet fsi tests/skchart3_test.fsx
```

And move the slider to interact with these charts. 
