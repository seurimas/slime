module Slime.Engine exposing (System(..), Engine, initEngine, applySystems)

{-|

-}

import Slime exposing (EntityDeletor)


type System world
    = TimeAndDeletes (Float -> world -> ( world, List Int ))
    | Time (Float -> world -> world)
    | Deletes (world -> ( world, List Int ))
    | Basic (world -> world)


type alias Engine world =
    { deleteEntity : EntityDeletor world
    , systems : List (System world)
    }


initEngine : EntityDeletor world -> List (System world) -> Engine world
initEngine deleteEntity systems =
    { deleteEntity = deleteEntity
    , systems = systems
    }


applySystem : EntityDeletor world -> System world -> world -> Float -> world
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


applySystems : Engine world -> world -> Float -> world
applySystems engine world deltaTime =
    List.foldr (\system -> \world -> applySystem engine.deleteEntity system world deltaTime) world engine.systems
