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
        i : int
        key : string
        trafo : Trafo3d
    }

[<ModelType>]
type Model =
    {
        [<TreatAsValue>]
        store : Option<Storage>


        botTrafos : HashMap<int,Trafo3d[]>
        clouds : HashMap<int,PcInstance>
        count : int
        activeRange : Range1i

        icp : bool
        spot : bool

        cameraState     : CameraControllerState
    }

module Model =
    let initial = 
        {
            store = None
            clouds = FSharp.Data.Adaptive.HashMap.empty
            count = 0
            activeRange = Range1i.Invalid
            icp = true
            spot = true

            botTrafos = HashMap.empty
            cameraState = FreeFlyController.initial
        }