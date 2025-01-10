### WARNING

This project uses `Avalonia.FuncUI` and has some really weird (broken?) behaviour with bindings and lifetime - patching

#### how to build

In folder **tests** there are some `.fsx` scripts
To use them, make sure to have the fonts folder from [KaTeX-repo](https://github.com/KaTeX/KaTeX).   
Just copy the fonts folder from KateX repository inside the tests folder.

```
git clone https://github.com/raidenXR/mkxkFS.git
cd mkxkFS/src/Renderer
dotnet build
cd tests
# copy the fonts folder here
dotnet fsi renderer_test.fsx
```

#### Known issues

There are some very serious issues with [Lifetimes](https://funcui.avaloniaui.net/view-basics/lifetime) and [Bindings](https://funcui.avaloniaui.net/view-basics/how-to-create-bindings)

that mess with the state of  `Avalonia.Control.Object`s  and break stuff.    
I.e if the notation button (on the top left) is pressed, setting the value of some `IWriteable<bool>` breaks the   
rendering of *SKChartControl*. Same thing happens for every `IWriteable<'T>`, so there rest of the `writtable.Set`   
have been *commented out*.   

Problems derive from `Renderer/src/Views.fs` and `Renderer/src/skchartcontrols.fs` files.   
If anyone is well-versed with Avalonia.FunUI and its very weird *Lifetimes pattern* and the way it does the   
rendering of the Visual tree, they might be able to figure out how to fix those issues.   

Another bizzare issue is while creating [Bindings](https://funcui.avaloniaui.net/view-basics/how-to-create-bindings) for the custom `SKChartControl`   
the bindings method fails to set *value* to the `SKChartControl.SKChart` property...   

while creating a *ContentControl* instead seems to work. Creating a *SKChartControl*   
and setting its *SKChart* property via the binding method definitely does not work   

```fs

      ContentControl.create [
          ContentControl.content (new SKChartControl(skchart))
      ]

      // SKChartControl.create [
      //     SKChartControl.width 400
      //     SKChartControl.height 400
      //     SKChartControl.chart skchart        # fails to assign value to SKChartControl.SKChart property
      //     SKChartControl.onSizeChanged (fun s ->
      //      if skchart.IsSKChart2 then
      //          c2.W <- float32 s.NewSize.Width
      //          c2.H <- float32 s.NewSize.Height
      //          c2.Update()
      //      if skchart.IsSKChart3 then
      //          c3.W <- float32 s.NewSize.Width
      //          c3.H <- float32 s.NewSize.Height
      //          c3.Update()
      //      )                                                 
      // ]
```
