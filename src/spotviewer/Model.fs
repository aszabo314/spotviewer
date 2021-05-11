namespace spotviewer.Model

open System
open FSharp.Data.Adaptive

open Aardvark.Base
open Aardvark.UI.Primitives
open Adaptify

open Aardvark.Geometry.Points
open Aardvark.Data.Points

type PcInstance =
    {
        key : string
        trafo : Trafo3d
    }

[<ModelType>]
type Model =
    {
        [<TreatAsValue>]
        store : Option<Storage>

        clouds : HashSet<PcInstance>

        cameraState     : CameraControllerState
    }

module Model =
    let initial = 
        {
            store = None
            clouds = FSharp.Data.Adaptive.HashSet.empty

            cameraState = FreeFlyController.initial
        }