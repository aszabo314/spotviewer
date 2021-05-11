open spotviewer

open System
open FSharp.Data.Adaptive

open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.Service
open Aardvark.UI
open Aardium
open Suave
open Suave.WebPart

type EmbeddedResources = EmbeddedResources
[<EntryPoint>]
let main args =
    Aardvark.Init()
    Aardium.init()

    let app = new OpenGlApplication()
    let rt = app.Runtime

    WebPart.startServerLocalhost 4321 [
        MutableApp.toWebPart' app.Runtime false (App.start (App.app rt))
        Reflection.assemblyWebPart typeof<EmbeddedResources>.Assembly
    ] |> ignore
    
    Aardium.run {
        title "Aardvark rocks \\o/"
        width 1440
        height 900
        url "http://localhost:4321/"
    }

    0