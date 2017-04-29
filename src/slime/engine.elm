module Slime.Engine exposing (..)

import Slime exposing (deletor, (&>), ComponentDeletor)


type System rec
    = TimeAndDeletes (Float -> rec -> ( rec, List Int ))
    | Time (Float -> rec -> rec)
    | Deletes (rec -> ( rec, List Int ))
    | Basic (rec -> rec)


type alias Engine rec =
    { deleteEntity : ComponentDeletor rec
    , systems : List (System rec)
    }


applySystem : ComponentDeletor rec -> System rec -> rec -> Float -> rec
applySystem deletor system world deltaTime =
    let
        sweep world deletes =
            List.foldr (flip deletor) world deletes
    in
        case system of
            TimeAndDeletes func ->
                let
                    ( steppedWorld, deletions ) =
                        func deltaTime world
                in
                    sweep steppedWorld deletions

            Time func ->
                func deltaTime world

            Deletes func ->
                let
                    ( steppedWorld, deletions ) =
                        func world
                in
                    sweep steppedWorld deletions

            Basic func ->
                func world


applySystems : Engine rec -> rec -> Float -> rec
applySystems engine world deltaTime =
    List.foldr (\system -> \world -> applySystem engine.deleteEntity system world deltaTime) world engine.systems
