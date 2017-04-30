module Slime.Engine exposing (System(..), Listener(..), Engine, initEngine, applySystems, applyMessage)

{-|

-}

import Slime exposing (EntityDeletor, EntityID)


type System world
    = TimeAndDeletes (Float -> world -> ( world, List EntityID ))
    | Time (Float -> world -> world)
    | Deletes (world -> ( world, List EntityID ))
    | Basic (world -> world)


type Listener world msg
    = Listen (msg -> world -> world)
    | ListenAndDelete (msg -> world -> ( world, List EntityID ))


type alias Engine world msg =
    { deleteEntity : EntityDeletor world
    , systems : List (System world)
    , listeners : List (Listener world msg)
    }


initEngine : EntityDeletor world -> List (System world) -> List (Listener world msg) -> Engine world msg
initEngine deleteEntity systems listeners =
    { deleteEntity = deleteEntity
    , systems = systems
    , listeners = listeners
    }


sweepDeletes : EntityDeletor world -> world -> List EntityID -> world
sweepDeletes deletes world =
    List.foldr (flip deletes) world


applySystem : EntityDeletor world -> System world -> world -> Float -> world
applySystem deletor system world deltaTime =
    case system of
        TimeAndDeletes func ->
            let
                ( steppedWorld, deletions ) =
                    func deltaTime world
            in
                sweepDeletes deletor steppedWorld deletions

        Time func ->
            func deltaTime world

        Deletes func ->
            let
                ( steppedWorld, deletions ) =
                    func world
            in
                sweepDeletes deletor steppedWorld deletions

        Basic func ->
            func world


applySystems : Engine world msg -> world -> Float -> world
applySystems engine world deltaTime =
    List.foldr (\system -> \world -> applySystem engine.deleteEntity system world deltaTime) world engine.systems


listen : EntityDeletor world -> Listener world msg -> world -> msg -> world
listen deletor listener world msg =
    case listener of
        Listen func ->
            func msg world

        ListenAndDelete func ->
            let
                ( steppedWorld, deletions ) =
                    func msg world
            in
                sweepDeletes deletor steppedWorld deletions


applyMessage : Engine world msg -> world -> msg -> world
applyMessage engine world msg =
    List.foldr (\listener -> \world -> listen engine.deleteEntity listener world msg) world engine.listeners
