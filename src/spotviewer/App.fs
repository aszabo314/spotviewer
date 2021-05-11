namespace spotviewer

open System
open FSharp.Data.Adaptive

open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Rendering.PointSet
open Aardvark.UI
open Aardvark.UI.Primitives

open Aardvark.Data.Points
open Aardvark.Geometry.Points

open spotviewer.Model
open System.Text

type Message =
    | SetStore of string
    | SetKeysAndTrafos of string
    | SetBotTrafos of string
    | CameraMessage of FreeFlyController.Message
    | ToggleIcp
    | ToggleSpot
    | SetActiveRange of Range1i

module Shader =
    open FShade
    type UniformScope with
        member x.SuperColor : V4d = x?SuperColor

    let mixy (v : Effects.Vertex) =
        fragment {
            let col = uniform.SuperColor
            let newCol = v.c * col
            return {
                v with c = newCol
            }
        }

module App =
    
    let update (m : Model) (msg : Message) =
        match msg with
            | SetActiveRange r -> 
                {m with activeRange = r}
            | ToggleIcp -> 
                {m with icp = not m.icp}
            | ToggleSpot -> 
                {m with spot = not m.spot}
            | SetStore s -> 
                let store = PointCloud.OpenStore(s, LruDictionary(1L <<< 30))
                {m with store = Some store}
            | SetKeysAndTrafos k -> 
                let mutable i = 0
                let clouds = 
                    File.readAllLines k
                    |> Array.choose (fun line -> 
                        try 
                            let splitted = line.Split([|' '|], 2)
                            let key = splitted.[0].Trim()
                            let trafo = splitted.[1].Trim() |> Trafo3d.Parse
                            let res = Some (i,{key = key; trafo = trafo; i = i})
                            i <- i+1
                            res
                        with e -> 
                            Log.error "couldn't parse trafo: %s %A" line e
                            None
                    ) |> FSharp.Data.Adaptive.HashMap.ofArray
                {m with clouds = clouds; count = i}
            | SetBotTrafos s -> 
                let botTrafos = 
                    File.readAllLines s 
                    |> Array.indexed
                    |> Array.choose (fun (i,l) -> 
                        try 
                            let trafos = 
                                l.Split([|';'|])
                                |> Array.map (fun t -> t.Trim() |> Trafo3d.Parse)
                            Some (i,trafos)
                        with e -> 
                            Log.error "couldnt parse bot trafo: %s %A" l e
                            None
                    ) |> HashMap.ofArray
                {m with botTrafos = botTrafos}
            | CameraMessage msg ->
                { m with cameraState = FreeFlyController.update m.cameraState msg }

    let view (rt : IRuntime) (m : AdaptiveModel) =

        let frustum = 
            Frustum.perspective 10.0 0.1 500.0 0.1
                |> AVal.constant
        
        let robotBoxes = 
            m.botTrafos 
            |> AMap.toAVal 
            |> AVal.map (fun botMap -> 
                botMap 
                |> HashMap.toArray
                |> Array.map (fun (i,trafos) -> 
                    trafos |> Array.map (fun trafo -> 
                        let finalTrafo = 
                            m.clouds 
                            |> AMap.tryFind i
                            |> AVal.bind (fun cloud -> 
                                match cloud with 
                                | None -> AVal.constant trafo
                                | Some cloud -> 
                                    m.icp |> AVal.map (fun icp -> 
                                        if icp then trafo * cloud.trafo
                                        else trafo
                                    )
                            )
                        let box = Box3d(V3d(-0.5,-0.25,-0.15), V3d(0.5,0.25,0.15))
                        Sg.box (AVal.constant C4b.Blue) (AVal.constant box)
                        |> Sg.trafo finalTrafo
                    )
                    |> Sg.ofArray
                    |> Sg.onOff (m.activeRange |> AVal.map (fun r -> r.Contains i))
                )
                |> Sg.ofArray
            )
            |> Sg.dynamic
            |> Sg.diffuseTexture DefaultTextures.checkerboard
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
                do! DefaultSurfaces.simpleLighting
                do! Shader.mixy
            }
            |> Sg.uniform "SuperColor" (AVal.constant <| C4d.Yellow.ToV4d())
            |> Sg.onOff m.spot

        let lodTreeInstances =
            ASet.bind (fun store -> 
                store |> Option.map (fun (store : Storage) -> 
                    m.clouds |> AMap.toASet |> ASet.choose (fun (i,c) -> 
                        let root = store.GetPointSet(c.key)
                        let root = root.Root.Value

                        LodTreeInstance.PointTreeNode.Create(
                            System.Guid(c.key),
                            c,
                            store.Cache,
                            Symbol.Create(c.key), 
                            (Guid.NewGuid(),fun _ _ -> MapExt.empty),
                            Similarity3d.Identity, None, None, 0, 
                            root
                        ) |> Option.map (fun i -> 
                            let us = 
                                MapExt.ofList [
                                    "ModelTrafo", (m.icp |> AVal.map (fun b -> if b then c.trafo else Trafo3d.Identity)) :> IAdaptiveValue
                                    "TreeActive", (m.activeRange |> AVal.map (fun r -> r.Contains c.i) :> IAdaptiveValue)
                                ]
                            {root = i; uniforms = us}
                        )
                    )
                ) |> Option.defaultValue ASet.empty
            ) m.store

        let sg (values : Aardvark.Service.ClientValues) = 
            let renderConfig : PointSetRenderConfig =
                {
                    runtime = rt
                    viewTrafo = values.viewTrafo
                    projTrafo = values.projTrafo
                    size = values.size
                    colors = AVal.constant false
                    pointSize = AVal.constant 10.0
                    planeFit = AVal.constant true
                    planeFitTol = AVal.constant 0.009
                    planeFitRadius = AVal.constant 7.0
                    ssao = AVal.constant true
                    diffuse = AVal.constant true
                    gamma = AVal.constant 1.0
                    lodConfig = Aardvark.SceneGraph.LodTreeRendering.LodTreeRenderConfig.simple
                    ssaoConfig = 
                        {
                            radius = AVal.constant 0.04
                            threshold = AVal.constant 0.1
                            sigma = AVal.constant 5.0
                            sharpness = AVal.constant 4.0
                            sampleDirections = AVal.constant 2
                            samples = AVal.constant 4
                        }
                    pickCallback = None
                }

            let pointSg = 
                Sg.pointSets renderConfig lodTreeInstances
                |> Sg.noEvents

            Sg.ofList [pointSg; robotBoxes]
                
        let dependencies = 
            [
                {name="nouislider.js";url="nouislider.js";kind=Script}
                {name="nouislider.css";url="nouislider.css";kind=Stylesheet}
            ]

        let sliderJs =
            m.count |> AVal.map (fun num -> 
                let js = 
                    sprintf """
                        (function() {
                        var s = document.getElementById('__ID__');

                        noUiSlider.create(s, {
                            start: [0, %d],
                            step: 1,
                            behaviour: 'drag',
                            connect: true,
                            range: {
                                'min': 0,
                                'max': %d
                            }
                        });

                        s.noUiSlider.on('update',function(e){aardvark.processEvent('__ID__', 'sliderUpdated', e)});
                        })();
                        """ num num
            
                require dependencies (
                    onBoot js (
                        div [
                            style "position:absolute;bottom:5px;left:1px;width:99vw;height:30px;background-color:lavenderblush"
                            onEvent "sliderUpdated" [] (fun l -> 
                                let fs : list<float> = l |> List.head |> Pickler.unpickleOfJson
                                SetActiveRange (Range1i(int fs.[0], int fs.[1]))
                            )
                        ] []
                    )
                )
            )

        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"
            ]

        body [] [
            FreeFlyController.controlledControlWithClientValues m.cameraState CameraMessage frustum (AttributeMap.ofList att) RenderControlConfig.standard sg

            div [style "position:absolute;top:1px;left:1px;background-color:lavenderblush"] [
                Primitives.SimplePrimitives.checkbox 
                    [style "margin-top:1px;margin-left:1px;margin-bottom:1px;margin-right:0px;top:0px;left:0px"]
                    m.icp
                    ToggleIcp
                    "ICP"
                Primitives.SimplePrimitives.checkbox 
                    [style "margin-top:1px;margin-left:1px;margin-bottom:1px;margin-right:0px;left:0px"]
                    m.spot
                    ToggleSpot
                    "spot"
            ]

            Incremental.div AttributeMap.empty (AList.ofAVal (sliderJs |> AVal.map List.singleton))

        ]

    let initial = 
        let inline u a b = update b a 
        Model.initial
        |> u (SetBotTrafos     @"C:\bla\stores\vrvis-ultra-individualChunks\botTrafos.txt")
        |> u (SetStore         @"C:\bla\stores\vrvis-ultra-individualChunks\store")
        |> u (SetKeysAndTrafos @"C:\bla\stores\vrvis-ultra-individualChunks\keys.txt")

    let app (rt : IRuntime) =
        {
            initial = initial
            update = update
            view = view rt
            threads = fun m -> m.cameraState |> FreeFlyController.threads |> ThreadPool.map CameraMessage
            unpersist = Unpersist.instance
        }
