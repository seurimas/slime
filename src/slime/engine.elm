module Slime.Engine exposing (Engine, initEngine, System(..), applySystems, Listener(..), applyListeners)

{-| Engine provides a simple way to manage a growing game. To do so, it needs to be provided with a few values:

    -- An entity deletor. This is composed via Slime.deleteEntity and Slime.(&->). (Entity creation is handled more lazily and therefor does not require a matching value)
    -- A list of Systems. These are explained further later, but they basically conform to the usual ECS model.
    -- A list of Listeners. These allow the world to be updated based on TEA messages.


# The Engine
@docs Engine, initEngine

# Systems
@docs System, applySystems

# Listeners
@docs Listener, applyListeners

-}

import Slime exposing (EntityDeletor, EntityID)


{-| A System constructed in this way will be used by Engine to update the world
with control over side effects (deleting entities and sending commands).

Each of the types has a different function signature for different options:
 * Time: Accepts deltaTime as its first argument.
 * Commands: The return value includes messages. The extra value is included as part of a tuple.
 * Deletes: The return value includes EntityIDs which should be deleted. The extra value is included as part of a tuple.

There is also a Basic, which has no options.
-}
type System world msg
    = TimeAndDeletes (Float -> world -> ( world, List EntityID ))
    | Time (Float -> world -> world)
    | Deletes (world -> ( world, List EntityID ))
    | TimeAndCommandsDeletes (Float -> world -> ( world, Cmd msg, List EntityID ))
    | TimeAndCommands (Float -> world -> ( world, Cmd msg ))
    | CommandsDeletes (world -> ( world, Cmd msg, List EntityID ))
    | Commands (world -> ( world, Cmd msg ))
    | Basic (world -> world)


{-| A Listener constructed in a similar way to Systems with a different purpose:
accepting messages through TEA. It has the same options as System, except Time.
-}
type Listener world msg
    = Listen (msg -> world -> world)
    | ListenAndDeletes (msg -> world -> ( world, List EntityID ))
    | ListenAndCommandsDeletes (msg -> world -> ( world, Cmd msg, List EntityID ))
    | ListenAndCommands (msg -> world -> ( world, Cmd msg ))


{-| The Engine type is used as the first argument of applySystems and applyListeners.

With the engine, update functions can be massively simplified, in a true ECS fashion.
-}
type alias Engine world msg =
    { deleteEntity : EntityDeletor world
    , systems : List (System world msg)
    , listeners : List (Listener world msg)
    }


{-| Initializes an Engine with a deletor, a list of systems, and a list of listeners.
-}
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


{-| A good way to use this is to curry the Engine out:
    runWorld = applySystems engine
And then later...
    (updatedWorld, cmd) = runWorld deltaTime
-}
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


listen : EntityDeletor world -> Listener world msg -> world -> msg -> ( world, Cmd msg )
listen deletor listener world msg =
    case listener of
        Listen func ->
            func msg world ! []

        ListenAndDeletes func ->
            let
                ( steppedWorld, deletions ) =
                    func msg world
            in
                sweepDeletes deletor steppedWorld deletions ! []

        ListenAndCommandsDeletes func ->
            let
                ( steppedWorld, commands, deletions ) =
                    func msg world
            in
                ( sweepDeletes deletor steppedWorld deletions
                , commands
                )

        ListenAndCommands func ->
            func msg world


{-| A good way to use this is to curry the Engine out and use it in the catch-all
of your update function:
    runWorld = applySystems engine
And then later...
    _ ->
        (updatedWorld, cmd) = runWorld msg
-}
applyListeners : Engine world msg -> world -> msg -> ( world, Cmd msg )
applyListeners engine world msg =
    let
        merge listener ( world, cmd ) =
            let
                ( newWorld, addedCommands ) =
                    listen engine.deleteEntity listener world msg
            in
                ( newWorld, Cmd.batch [ cmd, addedCommands ] )
    in
        List.foldl merge (world ! []) engine.listeners
