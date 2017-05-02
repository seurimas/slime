module Slime exposing (EntityID, EntitySet, ComponentSpec, ComponentSet, EntityDeletor, initComponents, initIdSource, deleteEntity, (&->), Entity, Entity2, Entity3, spawnEmpty, spawnEntity, spawnEntity2, setEntity, setEntity2, entities, entities2, getComponent, map, stepEntities, stepEntities2, spawnEntities, spawnEntities2)

{-| Experimental

This library provides an easy way to construct entity-component-system style
codeflow in Elm. It achieves this mostly by leveraging currying and getter/setters. For example:

    type alias World =
        EntitySet
            { transforms : ComponentSet Rect
            , balls : ComponentSet Ball
            }

    transforms : ComponentSpec Rect World
    transforms =
        { getter: .transforms,
        , setter: (\world -> \comps -> {world | transforms = comps})
        }


    balls : ComponentSpec Ball World
    balls =
        { getter: .balls,
        , setter: (\world -> \comps -> {world | balls = comps})
        }

    moveBalls : Float -> World -> World
    moveBalls delta =
        -- ...
          stepEntities2 balls transforms (\ent2 -> { ent2 | b = addVelocity ent2.a ent2.b delta })

stepEntities2 goes through all the balls (any entity with a ball component and transform component)
in this example and updates their location based on their velocity and the time elapsed.

Because moveBalls' type signature has no concept of the components involved, these systems can easily be
composed to operate in sequence to create an ECS Engine.

# Types
@docs EntityID, EntitySet, ComponentSpec, ComponentSet, EntityDeletor, Entity, Entity2, Entity3

# Updates and Maps
@docs map, stepEntities, stepEntities2

# Initialization
@docs initComponents, initIdSource

# Deletion
@docs deleteEntity, (&->)

# Retrieval
@docs entities, entities2, getComponent

# Updates
@docs setEntity, setEntity2

# Creation
@docs spawnEmpty, spawnEntity, spawnEntity2, spawnEntities, spawnEntities2
-}

import Array exposing (Array, length, append, push, get, set, repeat, indexedMap, toList)
import Dict exposing (Dict, remove)
import Array.Extra exposing (zip, zip3, filterMap)
import Lazy.List exposing (LazyList, numbers, drop, (+++), headAndTail, cons)


{-| A simple {getter, setter} record which is used as a building block for complex functions on `world`
-}
type alias ComponentSpec a world =
    { getter : world -> ComponentSet a
    , setter : world -> ComponentSet a -> world
    }


{-| -}
type alias EntityID =
    Int


{-| -}
type alias EntityDeletor world =
    world -> EntityID -> world


{-| -}
type alias EntitySetter a world =
    world -> Entity a -> world


{-| -}
type alias EntitySetter2 a b world =
    world -> Entity2 a b -> world


{-| -}
type alias EntitySetter3 a b c world =
    world -> Entity3 a b c -> world


type alias IDSource =
    { ids : LazyList EntityID
    , uids : LazyList EntityID
    , uidToId : Dict EntityID EntityID
    , idToUid : Dict EntityID EntityID
    }


{-| Your world should be an EntitySet. Include an `.idSource` initialized with `initIdSource`
-}
type alias EntitySet world =
    { world
        | idSource : IDSource
    }


{-| -}
type alias Entity a =
    { id : EntityID
    , a : a
    }


{-| -}
type alias Entity2 a b =
    { id : EntityID
    , a : a
    , b : b
    }


{-| -}
type alias Entity3 a b c =
    { id : EntityID
    , a : a
    , b : b
    , c : c
    }


{-| Include as a field in your world and initialize with `.initComponents`
-}
type alias ComponentSet a =
    { contents : Array (Maybe a)
    }


tag : EntityID -> Maybe a -> Maybe (Entity a)
tag i ma =
    case ma of
        Just a ->
            Just { id = i, a = a }

        _ ->
            Nothing


addB : b -> Entity a -> Entity2 a b
addB b e =
    { id = e.id
    , a = e.a
    , b = b
    }


tag2 : EntityID -> ( Maybe a, Maybe b ) -> Maybe (Entity2 a b)
tag2 i ( ma, mb ) =
    tag i ma
        |> Maybe.map2 addB mb


addC : c -> Entity2 a b -> Entity3 a b c
addC c e =
    { id = e.id
    , a = e.a
    , b = e.b
    , c = c
    }


tag3 : EntityID -> ( Maybe a, Maybe b, Maybe c ) -> Maybe (Entity3 a b c)
tag3 i ( ma, mb, mc ) =
    tag2 i ( ma, mb )
        |> Maybe.map2 addC mc



{-
   Initialization
-}


{-| Use to create component sets.
-}
initComponents : ComponentSet a
initComponents =
    { contents = Array.empty }


{-| Use to create an ID source for an EntitySet.
-}
initIdSource : IDSource
initIdSource =
    { ids = numbers
    , uids = Lazy.List.map ((*) -1) numbers
    , idToUid = Dict.empty
    , uidToId = Dict.empty
    }


{-| Takes an id and allows it to be used again in an IDSource.
-}
reclaimId : EntityID -> IDSource -> IDSource
reclaimId id ({ ids, uidToId, idToUid } as idSource) =
    { idSource
        | ids = cons id ids
        , uidToId = remove (Dict.get id idToUid |> Maybe.withDefault 0) uidToId
        , idToUid = remove id idToUid
    }


{-| Given an ID source, claims an id and an uid.
-}
claimId : IDSource -> ( IDSource, EntityID, EntityID )
claimId ({ ids, uids, uidToId, idToUid } as idSource) =
    let
        alwaysId =
            headAndTail ids

        alwaysUid =
            headAndTail uids
    in
        case alwaysId of
            Just ( id, remainingIds ) ->
                case alwaysUid of
                    Just ( uid, remainingUids ) ->
                        ( { idSource
                            | ids = remainingIds
                            , uids = remainingUids
                            , idToUid = Dict.insert id uid idToUid
                            , uidToId = Dict.insert uid id uidToId
                          }
                        , id
                        , uid
                        )

                    Nothing ->
                        ( idSource, 0, 0 )

            Nothing ->
                ( idSource, 0, 0 )


{-| Use as the start of a deletion block:

    deletor = deleteEntity transformSpec
        &-> massSpec
        &-> anotherSpec

The resulting deletor takes an EntityID and a world and clears the world of that Entity.
-}
deleteEntity : ComponentSpec a (EntitySet world) -> EntityDeletor (EntitySet world)
deleteEntity spec world id =
    let
        andReclaim id world =
            { world | idSource = reclaimId id world.idSource }
    in
        deleteComponent spec world id
            |> andReclaim id



{-
   Deletion
-}


deleteComponent : ComponentSpec a (EntitySet world) -> EntityDeletor (EntitySet world)
deleteComponent { getter, setter } world id =
    let
        delete index components =
            let
                newContents =
                    set index Nothing components.contents
            in
                { components | contents = newContents }
    in
        getter world
            |> delete id
            |> setter world


{-| -}
(&->) : EntityDeletor (EntitySet world) -> ComponentSpec b (EntitySet world) -> EntityDeletor (EntitySet world)
(&->) accumulatedDelete newSpec world index =
    accumulatedDelete world index
        |> flip (deleteComponent newSpec) index
infixl 1 &->



{-
   Spawning and updating
-}


{-| Spawns an empty Entity. Useful if you just need an Entity ID and want to
set the components manually.
-}
spawnEmpty : EntitySet world -> ( EntitySet world, EntityID, EntityID )
spawnEmpty entitySet =
    let
        ( updatedIdSource, newId, newUid ) =
            claimId entitySet.idSource
    in
        ( { entitySet | idSource = updatedIdSource }
        , newId
        , newUid
        )


{-| Spawns an Entity with one component.
-}
spawnEntity : ComponentSpec a (EntitySet world) -> EntitySet world -> { a : a } -> ( EntitySet world, EntityID, EntityID )
spawnEntity { getter, setter } entitySet { a } =
    let
        ( updatedSet, id, uid ) =
            spawnEmpty entitySet

        updatedComponents =
            getter updatedSet
                |> setComponent id a
    in
        ( setter updatedSet updatedComponents, id, uid )


{-| A convenience method to spawn multiple entities, provided as a list.
-}
spawnEntities : ComponentSpec a (EntitySet world) -> EntitySet world -> List { a : a } -> ( EntitySet world, List EntityID, List EntityID )
spawnEntities spec world ents =
    let
        merge spawn ( world, ids, uids ) =
            let
                ( newWorld, newSpawnId, newSpawnUid ) =
                    spawnEntity spec world spawn
            in
                ( newWorld, [ newSpawnId ] ++ ids, [ newSpawnUid ] ++ uids )
    in
        List.foldr merge ( world, [], [] ) ents


{-| Spawns an Entity with two components.
-}
spawnEntity2 : ComponentSpec a (EntitySet world) -> ComponentSpec b (EntitySet world) -> EntitySet world -> { a : a, b : b } -> ( EntitySet world, EntityID, EntityID )
spawnEntity2 specA specB entitySet { a, b } =
    let
        ( updatedSet, id, uid ) =
            spawnEmpty entitySet

        updatedComponentsA =
            specA.getter updatedSet
                |> setComponent id a

        updatedComponentsB =
            specB.getter updatedSet
                |> setComponent id b
    in
        ( specA.setter updatedSet updatedComponentsA |> flip specB.setter updatedComponentsB, id, uid )


{-| A convenience method to spawn multiple entities, provided as a list.
-}
spawnEntities2 : ComponentSpec a (EntitySet world) -> ComponentSpec b (EntitySet world) -> EntitySet world -> List { a : a, b : b } -> ( EntitySet world, List EntityID, List EntityID )
spawnEntities2 specA specB world ents =
    let
        merge spawn ( world, ids, uids ) =
            let
                ( newWorld, newSpawnId, newSpawnUid ) =
                    spawnEntity2 specA specB world spawn
            in
                ( newWorld, [ newSpawnId ] ++ ids, [ newSpawnUid ] ++ uids )
    in
        List.foldr merge ( world, [], [] ) ents


setComponent : EntityID -> a -> ComponentSet a -> ComponentSet a
setComponent index value components =
    if index - length components.contents < 0 then
        { components
            | contents =
                set index (Just value) components.contents
        }
    else
        { components
            | contents =
                append components.contents (repeat (index - length components.contents) Nothing)
                    |> push (Just value)
        }


stepComponents : (a -> a) -> ComponentSet a -> ComponentSet a
stepComponents update components =
    let
        newContents =
            Array.map (Maybe.map update) components.contents
    in
        { components | contents = newContents }


{-| Step entities based only on one component.
-}
stepEntities : ComponentSpec a world -> (a -> a) -> world -> world
stepEntities { getter, setter } update record =
    let
        components =
            getter record

        updatedComponents =
            stepComponents update components
    in
        setter record updatedComponents


{-| Step entities based on two component.
-}
stepEntities2 : ComponentSpec a world -> ComponentSpec b world -> (Entity2 a b -> Entity2 a b) -> world -> world
stepEntities2 specA specB update record =
    let
        entities =
            entities2 specA specB record

        updatedEntities =
            List.map update entities

        setter =
            setEntity2 specA specB
    in
        (List.foldr (flip setter) record updatedEntities)


{-| Step entities based on three component.
-}
stepEntities3 : ComponentSpec a world -> ComponentSpec b world -> ComponentSpec c world -> (Entity3 a b c -> Entity3 a b c) -> world -> world
stepEntities3 specA specB specC update record =
    let
        entities =
            entities3 specA specB specC record

        updatedEntities =
            List.map update entities

        setter =
            setEntity3 specA specB specC
    in
        (List.foldr (flip setter) record updatedEntities)


{-| Sets the world with an entity's component updated.
-}
setEntity : ComponentSpec a world -> world -> Entity a -> world
setEntity { getter, setter } record entity =
    let
        updatedComponents =
            getter record
                |> setComponent entity.id entity.a
    in
        setter record updatedComponents


{-| Sets the world with an entity's component updated.
-}
setEntity2 : ComponentSpec a world -> ComponentSpec b world -> world -> Entity2 a b -> world
setEntity2 specA specB record entity =
    let
        updatedComponentsA =
            specA.getter record
                |> setComponent entity.id entity.a

        updatedComponentsB =
            specB.getter record
                |> setComponent entity.id entity.b
    in
        specA.setter record updatedComponentsA
            |> flip specB.setter updatedComponentsB


{-| Sets the world with an entity's component updated.
-}
setEntity3 : ComponentSpec a world -> ComponentSpec b world -> ComponentSpec c world -> world -> Entity3 a b c -> world
setEntity3 specA specB specC record entity =
    let
        updatedComponentsA =
            specA.getter record
                |> setComponent entity.id entity.a

        updatedComponentsB =
            specB.getter record
                |> setComponent entity.id entity.b

        updatedComponentsC =
            specC.getter record
                |> setComponent entity.id entity.c
    in
        specA.setter record updatedComponentsA
            |> flip specB.setter updatedComponentsB
            |> flip specC.setter updatedComponentsC



{-
   Retrieval
-}


{-| Simple retrieval from a component set.
-}
getComponent : ComponentSet a -> EntityID -> Maybe a
getComponent components index =
    get index components.contents
        |> Maybe.andThen identity


completeEntities : Array (Maybe a) -> List a
completeEntities =
    (filterMap identity) >> toList


{-| Creates a list of something for each existing component of a given type.
-}
map : ComponentSpec a world -> (a -> b) -> world -> List b
map { getter } func record =
    getter record
        |> .contents
        |> filterMap identity
        |> toList
        |> List.map func


{-| Simple entity retrieval.
-}
entities : ComponentSpec a world -> world -> List (Entity a)
entities { getter } record =
    getter record
        |> .contents
        |> indexedMap tag
        |> completeEntities


{-| Simple entity retrieval. An entity is only included if both components provided exist for that entity.
-}
entities2 : ComponentSpec a world -> ComponentSpec b world -> world -> List (Entity2 a b)
entities2 specA specB record =
    zip ((.contents << specA.getter) record) ((.contents << specB.getter) record)
        |> indexedMap tag2
        |> completeEntities


{-| Simple entity retrieval. An entity is only included if all components provided exist for that entity.
-}
entities3 : ComponentSpec a world -> ComponentSpec b world -> ComponentSpec c world -> world -> List (Entity3 a b c)
entities3 specA specB specC record =
    zip3 ((.contents << specA.getter) record) ((.contents << specB.getter) record) ((.contents << specC.getter) record)
        |> indexedMap tag3
        |> completeEntities
