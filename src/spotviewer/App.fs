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
    | CameraMessage of FreeFlyController.Message

module App =
    
    let update (m : Model) (msg : Message) =
        match msg with
            | SetStore s -> 
                let store = PointCloud.OpenStore(s, LruDictionary(1L <<< 30))
                {m with store = Some store}
            | SetKeysAndTrafos k -> 
                let clouds = 
                    File.readAllLines k
                    |> Array.choose (fun line -> 
                        try 
                            let splitted = line.Split([|' '|], 2)
                            let key = splitted.[0].Trim()
                            let trafo = splitted.[1].Trim() |> Trafo3d.Parse
                            Some {key = key; trafo = trafo}
                        with _ -> 
                            Log.error "couldn't parse: %s" line
                            None
                    ) |> FSharp.Data.Adaptive.HashSet.ofArray
                {m with clouds = clouds}
            | CameraMessage msg ->
                { m with cameraState = FreeFlyController.update m.cameraState msg }

    let view (rt : IRuntime) (m : AdaptiveModel) =

        let frustum = 
            Frustum.perspective 10.0 0.1 500.0 0.1
                |> AVal.constant
        
        let lodTreeInstances =
            ASet.bind (fun store -> 
                store |> Option.map (fun (store : Storage) -> 
                    m.clouds |> ASet.choose (fun c -> 
                        let root = store.GetPointSet(c.key)
                        let root = root.Root.Value
                        LodTreeInstance.PointTreeNode.Create(
                            System.Guid(c.key),
                            c,
                            store.Cache,
                            Symbol.Create(c.key), 
                            (Guid.NewGuid(),fun _ _ -> MapExt.empty),
                            Similarity3d.FromTrafo3d c.trafo, None, None, 0, 
                            root
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


            let res = Sg.pointSets renderConfig (lodTreeInstances |> ASet.map (fun i -> {root = i; uniforms = MapExt.empty}))
                
            res |> Sg.noEvents

        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"
            ]

        body [] [
            FreeFlyController.controlledControlWithClientValues m.cameraState CameraMessage frustum (AttributeMap.ofList att) RenderControlConfig.standard sg
        ]

    let initial = 
        let inline u a b = update b a 
        Model.initial
        |> u (SetStore @"C:\bla\stores\vrvis-ultra-individualChunks\store")
        |> u (SetKeysAndTrafos @"C:\bla\stores\vrvis-ultra-individualChunks\keys.txt")

    let app (rt : IRuntime) =
        {
            initial = initial
            update = update
            view = view rt
            threads = fun m -> m.cameraState |> FreeFlyController.threads |> ThreadPool.map CameraMessage
            unpersist = Unpersist.instance
        }
