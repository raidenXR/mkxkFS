#r "../bin/Debug/net8.0/mkxkFS.dll"
#r "../bin/Debug/net8.0/Renderer.dll"

open System
open RendererFS

let renderer = GenericRenderer()
// let renderer = GenericRenderer(Views.viewTest)
renderer.Run()

Console.ReadKey()
