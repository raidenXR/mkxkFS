### Notation

this project parses simple TeX string and produces *.png* images of the mathematical formulas.   
On top of that, it **can** compile via AoT as native lib, and be utilized from other languages, besides .*NET*.   
On `tests/viewnotation_test.fsx` there is a demo app for showcasing this project.   
Run it with commands

```
cd tests
dotnet fsi viewnotation_test.fsx
```


#### Build

In folder **tests** there are some `.fsx` scripts
To use them, make sure to have the fonts folder from [KaTeX-repo](https://github.com/KaTeX/KaTeX).   
Just copy the fonts folder from KateX repository inside the tests folder.

```
git clone https://github.com/raidenXR/mkxkFS.git
cd mkxkFS/src/Notation
dotnet build
cd tests
# copy the fonts folder here
dotnet fsi viewnotation_test.fsx
```

A demo app (tests/viewnotation_test.fsx) can be run, after following the   
build instructions, to have a general idea about the resulting *.png* images   
of the lib and its limitations.   

#### Simple example

example how to use it, and create a .png image.  

```fs
open System
open System.IO
open Notation

let tex = @"f(x) = \frac{(C_{\beta}) - C_B}{(f(x)) / (\frac{C_A}{(N)}) \cdot (266.955 - +(C_a) * \exp(R / \sqrt{(\ln{C_{AB}} * \frac{\frac{((N_A))}{C_a}}{k_A}) + A_1}))}"

let fs = File.Create("sample.png")

tex
|> Typesetting.parseTeX
|> Typesetting.render false fs
fs.Flush()
fs.Close()
```

#### Build for UnmanagedCallers

```
dotnet publish -c Release -r linux-x64
# dotnet publish -c Release -r win-x64
# dotnet publish -c Release -r osx-x64
```

the native lib, will be in *bin/Release/net8.0/linux-x64/publish/* alongside  
`libSkiaSharp.so`. **Copy both**, to reference from the unmanaged caller.   
In *tests/zig_unmanaged_caller/* exists an example that calls the compiled F# lib   
from Zig, creating a *.png* image of some simple tex-string.  
***WARNING:*** in case of Zig and linux example, the Notation.so must be renamed to   
`libNotation.so` for the Zig build system to find. In addition create a **libs** folder   
and copy *libSkiaSharp.so* and *libNotation.so*.
i.e.
```
zig_unmanaged_caller/libs/libNotation.so
zig_unmanaged_caller/libs/libSkiaSharp.so
zig_unmanaged_caller/src/main.zig
zig_unmanaged_caller/build.zig
```

#### Build and run the 'zig unmanaged caller'

(Zig 0.14 stable is recommended)

```
cd tests/zig_unmanaged_caller
zig build
cd zig-out/bin
# copy the fonts folder in this directory before executing the program
# copy the libs folder in this directory before executing the program
./zig_unmanaged_caller
ls -l
```
A *tex_image.png* was produced from the libNotation.so, invoked from Zig!!   
   
   
