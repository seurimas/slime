module Slime.Engine exposing (System(..), Listener(..), Engine, initEngine, applySystems, applyMessage)

{-|

-}

import Slime exposing (EntityDeletor, EntityID)


type System world msg
    = TimeAndDeletes (Float -> world -> ( world, List EntityID ))
    | Time (Float -> world -> world)
    | Deletes (world -> ( world, List EntityID ))
    | TimeAndCommandsDeletes (Float -> world -> ( world, Cmd msg, List EntityID ))
    | TimeAndCommands (Float -> world -> ( world, Cmd msg ))
    | CommandsDeletes (world -> ( world, Cmd msg, List EntityID ))
    | Commands (world -> ( world, Cmd msg ))
    | Basic (world -> world)


type Listener world msg
    = Listen (msg -> world -> world)
    | ListenAndDelete (msg -> world -> ( world, List EntityID ))


type alias Engine world msg =
    { deleteEntity : EntityDeletor world
    , systems : List (System world msg)
    , listeners : List (Listener world msg)
    }


initEngine : EntityDeletor world -> List (System world msg) -> List (Listener world msg) -> Engine world msg
initEngine deleteEntity systems listeners =
    { deleteEntity = deleteEntity
    , systems = systems
    , listeners = listeners
    }


sweepDeletes : EntityDeletor world -> world -> List EntityID -> world
sweepDeletes deletes world =
    List.foldr (flip deletes) world


applySystem : EntityDeletor world -> System world msg -> world -> Float -> ( world, Cmd msg )
applySystem deletor system world deltaTime =
    case system of
        TimeAndDeletes func ->
            let
                ( steppedWorld, deletions ) =
                    func deltaTime world
            in
                sweepDeletes deletor steppedWorld deletions ! []

        Time func ->
            func deltaTime world ! []

        Deletes func ->
            let
                ( steppedWorld, deletions ) =
                    func world
            in
                sweepDeletes deletor steppedWorld deletions ! []

        TimeAndCommandsDeletes func ->
            let
                ( steppedWorld, commands, deletions ) =
                    func deltaTime world
            in
                ( sweepDeletes deletor steppedWorld deletions
                , commands
                )

        TimeAndCommands func ->
            func deltaTime world

        CommandsDeletes func ->
            let
                ( steppedWorld, commands, deletions ) =
                    func world
            in
                ( sweepDeletes deletor steppedWorld deletions
                , commands
                )

        Commands func ->
            func world

        Basic func ->
            func world ! []


applySystems : Engine world msg -> world -> Float -> ( world, Cmd msg )
applySystems engine world deltaTime =
    let
        merge system ( world, cmd ) =
            let
                ( newWorld, addedCommands ) =
                    applySystem engine.deleteEntity system world deltaTime
            in
                ( newWorld, Cmd.batch [ cmd, addedCommands ] )
    in
        List.foldl merge (world ! []) engine.systems


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
    List.foldl (\listener -> \world -> listen engine.deleteEntity listener world msg) world engine.listeners
