### MKXK-Viewer

Is a tool for visualing the evaluation of the ASTs alongside their mathematical notation.   

It can interchange the type of the chart between 2d line/points chart to a 3d surface
regarding the selected variables as X & Y axis, and dynamically recalculating the   
functions while resetting the coresponding values with the sliders.

[mkxk-viewer_sample.mp4](https://github.com/raidenXR/mkxkFS/tree/main/videos)

For more details regarding the API, read the **tests/mkxk_app_test.fsx** example.

Create `Expr` objects same way as in MKXK/README.md and pass these objects a     
parameter in some Models.create function. Then render the UI tool alongside these   
formulas with the Renderer.RunParallel function.

```fs
let models = [
    "f1(x)", Models.createTeXModel maps fn1 "C_A" Colors.OrangeRed 2.0f
    "f2(x)", Models.createTeXModel maps fn2 "C_A" Colors.Fuchsia 2.0f
]

let renderer = Renderer("MKXK-Viewer")
renderer.RunParallel(fun _ -> Views.view2(maps, models))
Console.ReadKey()
```

### How to build

In folder **tests** there are some `.fsx` scripts
To use them, make sure to have the fonts folder from [KaTeX-repo](https://github.com/KaTeX/KaTeX).   
Just copy the fonts folder from KateX repository inside the tests folder.

```
git clone https://github.com/raidenXR/mkxkFS.git
cd mkxkFS/src/MKXK-Viewer
dotnet build -c Release
cd tests
# copy the fonts folder here
dotnet fsi renderer_test.fsx
```




