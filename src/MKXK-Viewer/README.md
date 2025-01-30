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

