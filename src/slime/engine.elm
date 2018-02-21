module Slime.Engine
    exposing
        ( Engine
        , initEngine
        , Message(..)
        , noop
        , engineSubs
        , timed
        , untimed
        , noOptions
        , cmds
        , deletes
        , cmdsAndDeletes
        , System
        , timedSystem
        , untimedSystem
        , systemWith
        , systemMap
        , applySystems
        , Listener
        , listener
        , listenerWith
        , listenerMap
        , applyListeners
        , engineUpdate
        )

{-| Engine provides a simple way to manage a growing game. To do so, it needs to be provided with a few values:

    -- An entity deletor. This is composed via Slime.deleteEntity and Slime.(&->). (Entity creation is handled more lazily and therefor does not require a matching value)
    -- A list of Systems. These are explained further later, but they basically conform to the usual ECS model.
    -- A list of Listeners. These allow the world to be updated based on TEA messages.


# The Engine

@docs Engine, initEngine, engineSubs, engineUpdate, Message, noop


# System and listener options

@docs timed, untimed, noOptions, cmds, deletes, cmdsAndDeletes


# Systems

@docs System, untimedSystem, timedSystem, systemWith, systemMap


# Listeners

@docs Listener, listener, listenerWith, listenerMap


# Manual methods

@docs applyListeners, applySystems

-}

import Slime exposing (EntityDeletor, EntityID)
import AnimationFrame


type alias SystemStep world msg =
    { updatedWorld : world
    , commands : Cmd msg
    , deletes : List EntityID
    }


worldStep : world -> SystemStep world msg
worldStep world =
    { updatedWorld = world
    , commands = Cmd.none
    , deletes = []
    }


worldCmdsStep : ( world, Cmd msg ) -> SystemStep world msg
worldCmdsStep ( world, commands ) =
    { updatedWorld = world
    , commands = commands
    , deletes = []
    }


worldDeletesStep : ( world, List EntityID ) -> SystemStep world msg
worldDeletesStep ( world, deletes ) =
    { updatedWorld = world
    , commands = Cmd.none
    , deletes = deletes
    }


worldCmdsDeletesStep : ( world, Cmd msg, List EntityID ) -> SystemStep world msg
worldCmdsDeletesStep ( world, commands, deletes ) =
    { updatedWorld = world
    , commands = commands
    , deletes = deletes
    }


applySystemStep : EntityDeletor world -> SystemStep world msg -> ( world, Cmd msg )
applySystemStep deletor { updatedWorld, commands, deletes } =
    ( sweepDeletes deletor updatedWorld deletes
    , commands
    )


{-| A System constructed in this way will be used by Engine to update the world
with control over side effects (deleting entities and sending commands).

Each of the types has a different function signature for different options:

  - Time: Accepts deltaTime as its first argument.
  - Commands: The return value includes messages. The extra value is included as part of a tuple.
  - Deletes: The return value includes EntityIDs which should be deleted. The extra value is included as part of a tuple.

There is also a Basic, which has no options.

-}
type System world msg
    = System (Float -> world -> SystemStep world msg)


{-| Use in systemWith/listenerWith to define a system or listener with no commands or deletes.
-}
noOptions : world -> SystemStep world msg
noOptions =
    worldStep


{-| Use in systemWith/listenerWith to define a system or listener with commands.
-}
cmds : ( world, Cmd msg ) -> SystemStep world msg
cmds =
    worldCmdsStep


{-| Use in systemWith/listenerWith to define a system or listener with deletes.
-}
deletes : ( world, List EntityID ) -> SystemStep world msg
deletes =
    worldDeletesStep


{-| Use in systemWith/listenerWith to define a system or listener with commands and deletes.
-}
cmdsAndDeletes : ( world, Cmd msg, List EntityID ) -> SystemStep world msg
cmdsAndDeletes =
    worldCmdsDeletesStep


{-| Use in systemWith to define a timed system.
-}
timed : (Float -> world -> step) -> (Float -> world -> step)
timed =
    identity


{-| Use in systemWith to define an untimed system.
-}
untimed : (world -> step) -> (Float -> world -> step)
untimed system dt =
    system


{-| Creates a system that does not accept deltas, creates no commands or deletes.
-}
untimedSystem : (world -> world) -> System world msg
untimedSystem =
    systemWith
        { timing = untimed
        , options = noOptions
        }


{-| Creates a system that accepts deltas, creates no commands or deletes.
-}
timedSystem : (Float -> world -> world) -> System world msg
timedSystem =
    systemWith
        { timing = timed
        , options = noOptions
        }


{-| Creates a system with particular timing (timed or untimed) and options (e.g.
cmds, deletes, cmdsAndDeletes).
-}
systemWith : { timing : baseSystem -> (Float -> world -> stepType), options : stepType -> SystemStep world msg } -> baseSystem -> System world msg
systemWith { timing, options } base =
    System (\dt world -> (timing base) dt world |> options)


{-| Maps a listener of one message type to another.
-}
systemMap : (originalMsg -> msg) -> System world originalMsg -> System world msg
systemMap msgMap (System system) =
    System
        (\dt world ->
            let
                ({ commands } as systemStep) =
                    system dt world
            in
                { systemStep
                    | commands = Cmd.map msgMap commands
                }
        )


{-| A Listener constructed in a similar way to Systems with a different purpose:
accepting messages through TEA. It has the same options as System, except Time.
-}
type Listener world msg
    = Listener (msg -> world -> SystemStep world msg)


{-| Creates a listener with no options (no commands are created and no deletes).
-}
listener : (msg -> world -> world) -> Listener world msg
listener listener =
    Listener (\msg world -> worldStep (listener msg world))


{-| Creates a listener with the given options (e.g. cmds, deletes, cmdsAndDeletes)
-}
listenerWith : { options : stepType -> SystemStep world msg } -> (msg -> world -> stepType) -> Listener world msg
listenerWith { options } base =
    Listener (\msg world -> (base msg world |> options))


{-| Maps a listener of one message type to another.
-}
listenerMap : (msg -> subMsg) -> (subMsg -> msg) -> Listener world subMsg -> Listener world msg
listenerMap toSub fromSub (Listener listener) =
    Listener
        (\msg world ->
            let
                ({ commands } as systemStep) =
                    listener (toSub msg) world
            in
                { systemStep
                    | commands = Cmd.map fromSub commands
                }
        )


{-| The Engine type is used as the first argument of applySystems and applyListeners.

With the engine, update functions can be massively simplified, in a true ECS fashion.

-}
type Engine world msg
    = Engine
        { deleteEntity : EntityDeletor world
        , systems : List (System world msg)
        , listeners : List (Listener world msg)
        }


{-| Initializes an Engine with a deletor, a list of systems, and a list of listeners.
-}
initEngine : EntityDeletor world -> List (System world msg) -> List (Listener world msg) -> Engine world msg
initEngine deleteEntity systems listeners =
    Engine
        { deleteEntity = deleteEntity
        , systems = systems
        , listeners = listeners
        }


sweepDeletes : EntityDeletor world -> world -> List EntityID -> world
sweepDeletes deletes world =
    List.foldr deletes world


applySystem : EntityDeletor world -> System world msg -> world -> Float -> ( world, Cmd msg )
applySystem deletor (System system) world deltaTime =
    let
        systemStep =
            system deltaTime world
    in
        applySystemStep deletor systemStep


{-| Useful if you write your own update method. Uses systems as defined in the
provided engine.
-}
applySystems : Engine world msg -> world -> Float -> ( world, Cmd msg )
applySystems (Engine engine) world deltaTime =
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
listen deletor (Listener listener) world msg =
    let
        systemStep =
            listener msg world
    in
        applySystemStep deletor systemStep


{-| Useful if you write your own update method. Uses listeners as defined in the
provided engine.
-}
applyListeners : Engine world msg -> world -> msg -> ( world, Cmd msg )
applyListeners (Engine engine) world msg =
    let
        merge listener ( world, cmd ) =
            let
                ( newWorld, addedCommands ) =
                    listen engine.deleteEntity listener world msg
            in
                ( newWorld, Cmd.batch [ cmd, addedCommands ] )
    in
        List.foldl merge (world ! []) engine.listeners


{-| Wraps messages and ticks.
-}
type Message msg
    = Tick Float
    | Msg msg
    | Noop


{-| For use when you need to pass a do-nothing message.
-}
noop : Message msg
noop =
    Noop


{-| Takes the subscriptions for your app and wraps them for the engine. Time messages are provided by the engine.

For use with engineUpdate.

Example:
Sub.batch [ ... ]
|> engineSubs

-}
engineSubs : Sub msg -> Sub (Message msg)
engineSubs subs =
    Sub.batch
        [ Sub.map Msg subs
        , AnimationFrame.diffs Tick
        ]


{-| Wraps up the engine in such a way that you can use it as your entire update function, if desired. Requires use of engineSubs.
-}
engineUpdate : Engine world msg -> Message msg -> world -> ( world, Cmd (Message msg) )
engineUpdate engine msg world =
    case msg of
        Tick delta ->
            let
                deltaMs =
                    delta / 1000

                ( updatedWorld, commands ) =
                    applySystems engine world deltaMs
            in
                ( updatedWorld
                , Cmd.map Msg commands
                )

        Msg msg ->
            let
                ( updatedWorld, commands ) =
                    applyListeners engine world msg
            in
                ( updatedWorld
                , Cmd.map Msg commands
                )

        Noop ->
            world ! []
