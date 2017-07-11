module Slime
    exposing
        ( EntityID
        , EntitySet
        , ComponentSpec
        , EntitySpec
        , ComponentSet
        , EntityDeletor
        , initComponents
        , initIdSource
        , getUidFromId
        , getIdFromUid
        , getEntityByUid
        , getEntity2ByUid
        , getEntity3ByUid
        , deleteEntity
        , (&->)
        , Entity
        , Entity2
        , Entity3
        , spawnEmpty
        , spawnEntity
        , spawnEntity2
        , setEntity
        , setEntity2
        , setEntity3
        , getEntities
        , getEntities2
        , getEntities3
        , (&.)
        , entities
        , entities2
        , entities3
        , stepEntities
        , stepEntitiesWith
        , getComponentById
        , getComponent
        , hasComponent
        , entityExistsByUid
        , map
        , spawnEntities
        , spawnEntities2
        , forEntityById
        , forEntityByUid
        , forNewEntity
        , (&=>)
        , (&~>)
        )

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
        , setter: (\comps world -> {world | transforms = comps})
        }


    balls : ComponentSpec Ball World
    balls =
        { getter: .balls,
        , setter: (\comps world -> {world | balls = comps})
        }

    moveBalls : Float -> World -> World
    moveBalls delta =
        -- ...
          stepEntities (entities2 balls transforms) (\ent2 -> { ent2 | b = addVelocity ent2.a ent2.b delta })

stepEntities goes through all the balls (any entity with a ball component and transform component)
in this example and updates their location based on their velocity and the time elapsed.

Because moveBalls' type signature has no concept of the components involved, these systems can easily be
composed to operate in sequence to create an ECS Engine.

# Types
@docs EntityID, EntitySet, ComponentSpec, EntitySpec, ComponentSet, EntityDeletor, Entity, Entity2, Entity3

# Entity specs
@docs entities, entities2, entities3

# Updates and Maps
@docs map, stepEntities, stepEntitiesWith

# Initialization
@docs initComponents, initIdSource

# Deletion
@docs deleteEntity, (&->)

# Retrieval
@docs getEntities, getEntities2, getEntities3, (&.), getEntityByUid, getEntity2ByUid, getEntity3ByUid, getComponentById, getComponent, hasComponent

# Entity management
@docs entityExistsByUid, getUidFromId, getIdFromUid

# Updates
@docs setEntity, setEntity2, setEntity3, forEntityById, forEntityByUid, (&=>), (&~>)

# Creation
@docs spawnEmpty, spawnEntity, spawnEntity2, spawnEntities, spawnEntities2, forNewEntity
-}

import Array exposing (Array, length, append, push, get, set, repeat, indexedMap, toList)
import Dict exposing (Dict, remove)
import Array.Extra exposing (zip, zip3, filterMap)
import Lazy.List exposing (LazyList, numbers, drop, (+++), headAndTail, cons)


{-| A simple {getter, setter} record which is used as a building block for complex functions on `world`
-}
type alias ComponentSpec a world =
    { getter : world -> ComponentSet a
    , setter : ComponentSet a -> world -> world
    }


{-| -}
type alias EntityID =
    Int


{-| -}
type alias EntityDeletor world =
    EntityID -> world -> world


{-| -}
type alias EntitySetter a world =
    Entity a -> world -> world


{-| -}
type alias EntitySetter2 a b world =
    Entity2 a b -> world -> world


{-| -}
type alias EntitySetter3 a b c world =
    Entity3 a b c -> world -> world


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
type alias Tagged r =
    { r | id : EntityID }


{-| -}
type alias EntitySpec ent world =
    { getter : world -> List (Tagged ent)
    , setter : List (Tagged ent) -> world -> world
    }


{-| -}
type alias Entity a =
    Tagged { a : a }


{-| -}
type alias Entity2 a b =
    Tagged { a : a, b : b }


{-| -}
type alias Entity3 a b c =
    Tagged { a : a, b : b, c : c }


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


tag2 : EntityID -> ( Maybe a, Maybe b ) -> Maybe (Entity2 a b)
tag2 i ( ma, mb ) =
    case ( ma, mb ) of
        ( Just a, Just b ) ->
            Just { id = i, a = a, b = b }

        _ ->
            Nothing


tag3 : EntityID -> ( Maybe a, Maybe b, Maybe c ) -> Maybe (Entity3 a b c)
tag3 i ( ma, mb, mc ) =
    case ( ma, mb, mc ) of
        ( Just a, Just b, Just c ) ->
            Just { id = i, a = a, b = b, c = c }

        _ ->
            Nothing



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


{-|
-}
entityExistsByUid : EntitySet world -> EntityID -> Bool
entityExistsByUid world id =
    getIdFromUid world id /= Nothing


{-| Retrieves an entity ID based on a permanent UID.
-}
getUidFromId : EntitySet world -> EntityID -> Maybe EntityID
getUidFromId world id =
    Dict.get id world.idSource.idToUid


{-| Retrieves a permanent UID from an entity ID.
-}
getIdFromUid : EntitySet world -> EntityID -> Maybe EntityID
getIdFromUid world uid =
    Dict.get uid world.idSource.uidToId


{-| Returns an entity from a permanent UID.
-}
getEntityByUid : ComponentSpec a (EntitySet world) -> EntitySet world -> EntityID -> Maybe (Entity (Maybe a))
getEntityByUid { getter } world uid =
    getUidFromId world uid
        |> Maybe.map
            (\id ->
                { id = id
                , a = getComponent (getter world) id
                }
            )


{-| Returns an entity from a permanent UID.
-}
getEntity2ByUid : ComponentSpec a (EntitySet world) -> ComponentSpec b (EntitySet world) -> EntitySet world -> EntityID -> Maybe (Entity2 (Maybe a) (Maybe b))
getEntity2ByUid specA specB world uid =
    getUidFromId world uid
        |> Maybe.map
            (\id ->
                { id = id
                , a = getComponent (specA.getter world) id
                , b = getComponent (specB.getter world) id
                }
            )


{-| Returns an entity from a permanent UID.
-}
getEntity3ByUid : ComponentSpec a (EntitySet world) -> ComponentSpec b (EntitySet world) -> ComponentSpec c (EntitySet world) -> EntitySet world -> EntityID -> Maybe (Entity3 (Maybe a) (Maybe b) (Maybe c))
getEntity3ByUid specA specB specC world uid =
    getUidFromId world uid
        |> Maybe.map
            (\id ->
                { id = id
                , a = getComponent (specA.getter world) id
                , b = getComponent (specB.getter world) id
                , c = getComponent (specC.getter world) id
                }
            )


{-| Use as the start of a deletion block:

    deletor = deleteEntity transformSpec
        &-> massSpec
        &-> anotherSpec

The resulting deletor takes an EntityID and a world and clears the world of that Entity.
-}
deleteEntity : ComponentSpec a (EntitySet world) -> EntityDeletor (EntitySet world)
deleteEntity spec id world =
    let
        andReclaim id world =
            { world | idSource = reclaimId id world.idSource }
    in
        deleteComponent spec id world
            |> andReclaim id



{-
   Deletion
-}


deleteComponent : ComponentSpec a (EntitySet world) -> EntityDeletor (EntitySet world)
deleteComponent { getter, setter } id world =
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
            |> flip setter world


{-| -}
(&->) : EntityDeletor (EntitySet world) -> ComponentSpec b (EntitySet world) -> EntityDeletor (EntitySet world)
(&->) accumulatedDelete newSpec index world =
    accumulatedDelete index world
        |> (deleteComponent newSpec) index
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
        ( setter updatedComponents updatedSet, id, uid )


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
        ( specA.setter updatedComponentsA updatedSet |> specB.setter updatedComponentsB, id, uid )


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


setEntities : (Tagged a -> world -> world) -> List (Tagged a) -> world -> world
setEntities setter updatedEntities record =
    List.foldr setter record updatedEntities


{-| Steps the entities in a world based on the entity spec and update function provided.
-}
stepEntities : EntitySpec ent world -> (Tagged ent -> Tagged ent) -> world -> world
stepEntities { getter, setter } updateEnt world =
    let
        ents =
            getter world

        updatedEnts =
            (List.map updateEnt) ents
    in
        setter updatedEnts world


{-| Step entities with an extra parameter. Useful for passing through an accumulator
(e.g. a counter or a random number generator).
-}
stepEntitiesWith : EntitySpec ent world -> (( Tagged ent, x ) -> ( Tagged ent, x )) -> ( world, x ) -> ( world, x )
stepEntitiesWith { getter, setter } updateWith ( world, extra ) =
    let
        ents =
            getter world

        folder nextEntity ( runningEntities, currentExtra ) =
            let
                ( updatedEntity, steppedExtra ) =
                    updateWith ( nextEntity, currentExtra )
            in
                ( updatedEntity :: runningEntities, steppedExtra )

        ( updatedEntities, updatedExtra ) =
            List.foldl folder ( [], extra ) ents
    in
        ( setter updatedEntities world
        , updatedExtra
        )


{-| Filters a step entity block. Example:

    stepGravity world =
        (stepEntities2 transform velocity
            |> filtered
                (\ent2 -> getComponentById antigrav ent2.id world == Nothing)
                applyGravity)
            world
-}
filtered :
    (Tagged x -> Bool)
    -> (Tagged x -> Tagged x)
    -> ((Tagged x -> Tagged x) -> world -> world)
    -> world
    -> world
filtered check updateFunc stepEnts world =
    let
        filteredUpdate entity =
            case check entity of
                True ->
                    updateFunc entity

                False ->
                    entity
    in
        stepEnts filteredUpdate world


{-| Filter or helper function to check if a component exists for a given entity
-}
hasComponent : ComponentSpec a world -> Tagged x -> world -> Bool
hasComponent { getter } ent world =
    getComponent (getter world) ent.id /= Nothing


{-| Sets the world with an entity's component updated.
-}
setEntity : ComponentSpec a world -> Entity a -> world -> world
setEntity { getter, setter } entity record =
    let
        updatedComponents =
            getter record
                |> setComponent entity.id entity.a
    in
        setter updatedComponents record


{-| Sets the world with an entity's component updated.
-}
setEntity2 : ComponentSpec a world -> ComponentSpec b world -> Entity2 a b -> world -> world
setEntity2 specA specB entity record =
    let
        updatedComponentsA =
            specA.getter record
                |> setComponent entity.id entity.a

        updatedComponentsB =
            specB.getter record
                |> setComponent entity.id entity.b
    in
        specA.setter updatedComponentsA record
            |> specB.setter updatedComponentsB


{-| Sets the world with an entity's component updated.
-}
setEntity3 : ComponentSpec a world -> ComponentSpec b world -> ComponentSpec c world -> Entity3 a b c -> world -> world
setEntity3 specA specB specC entity record =
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
        specA.setter updatedComponentsA record
            |> specB.setter updatedComponentsB
            |> specC.setter updatedComponentsC



{-
   Retrieval
-}


{-| Try to prefer getEntity(#)ByUid where appropriate. This is useful for filters, however.
-}
getComponentById : ComponentSpec a world -> EntityID -> world -> Maybe a
getComponentById { getter } id world =
    getComponent (getter world) id


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
getEntities : ComponentSpec a world -> world -> List (Entity a)
getEntities { getter } record =
    getter record
        |> .contents
        |> indexedMap tag
        |> completeEntities


{-| Simple entity retrieval. An entity is only included if both components provided exist for that entity.
-}
getEntities2 : ComponentSpec a world -> ComponentSpec b world -> world -> List (Entity2 a b)
getEntities2 specA specB record =
    zip ((.contents << specA.getter) record) ((.contents << specB.getter) record)
        |> indexedMap tag2
        |> completeEntities


{-| Simple entity retrieval. An entity is only included if all components provided exist for that entity.
-}
getEntities3 : ComponentSpec a world -> ComponentSpec b world -> ComponentSpec c world -> world -> List (Entity3 a b c)
getEntities3 specA specB specC record =
    zip3 ((.contents << specA.getter) record) ((.contents << specB.getter) record) ((.contents << specC.getter) record)
        |> indexedMap tag3
        |> completeEntities


{-| Convenience notation for retrieving based on an EntitySpec
-}
(&.) : world -> EntitySpec ent world -> List (Tagged ent)
(&.) world { getter } =
    getter world


{-| Defines an entity spec based on one component. These are all entities which have that component.
-}
entities : ComponentSpec a world -> EntitySpec { a : a } world
entities specA =
    { getter = getEntities specA
    , setter = setEntities (setEntity specA)
    }


{-| Defines an entity spec based on two components.
These are all entities which have both component.
-}
entities2 : ComponentSpec a world -> ComponentSpec b world -> EntitySpec { a : a, b : b } world
entities2 specA specB =
    { getter = getEntities2 specA specB
    , setter = setEntities (setEntity2 specA specB)
    }


{-| Defines an entity spec based on three components.
These are all entities which have all component.
-}
entities3 : ComponentSpec a world -> ComponentSpec b world -> ComponentSpec c world -> EntitySpec { a : a, b : b, c : c } world
entities3 specA specB specC =
    { getter = getEntities3 specA specB specC
    , setter = setEntities (setEntity3 specA specB specC)
    }


{-| Begin a chain of sets for a particular entity id.
-}
forEntityById : EntityID -> world -> ( Maybe EntityID, world )
forEntityById id world =
    ( Just id, world )


{-| Begin a chain of sets for a particular entity id.
-}
forEntityByUid : EntityID -> EntitySet world -> ( Maybe EntityID, EntitySet world )
forEntityByUid uid world =
    getIdFromUid world uid
        |> (flip (,)) world


{-| Begins a chain of sets for a new entity. Useful for complex entity spawning.
Example:

    (_, updatedWorld) =
        forNewEntity world
            &=> (locations (0, 0))
            &=> (place (2, 6))
-}
forNewEntity : EntitySet world -> ( Maybe EntityID, EntitySet world )
forNewEntity world =
    let
        ( updatedWorld, newId, _ ) =
            spawnEmpty world
    in
        ( Just newId, updatedWorld )


{-| Sets a particular entity's component. Used with forEntityById and forEntityByUid.
Example:
    (_, updatedWorld) =
        forEntityById id world
            &=> (locations, (0, 0))
            &=> (sizes, (2, 6))

-}
(&=>) : ( Maybe EntityID, world ) -> ( ComponentSpec a world, a ) -> ( Maybe EntityID, world )
(&=>) ( mid, world ) ( { getter, setter }, component ) =
    case mid of
        Just id ->
            let
                updatedComponents =
                    getter world
                        |> setComponent id component

                updatedWorld =
                    setter updatedComponents world
            in
                ( mid, updatedWorld )

        Nothing ->
            ( mid, world )


{-| Updates a particular entity's component. Used with forEntityById and forEntityByUid.
    (_, updatedWorld) =
        forEntityById id world
            &=> (locations, moveMe)
            &=> (sizes, shrinkMe)

-}
(&~>) : ( Maybe EntityID, EntitySet world ) -> ( ComponentSpec a (EntitySet world), Maybe a -> Maybe a ) -> ( Maybe EntityID, EntitySet world )
(&~>) ( mid, world ) ( { getter, setter } as spec, update ) =
    case mid of
        Just id ->
            let
                component =
                    getComponent (getter world) id

                updatedComponent =
                    update component

                updatedWorld =
                    case updatedComponent of
                        Just updated ->
                            setComponent id updated (getter world)
                                |> flip setter world

                        Nothing ->
                            deleteComponent spec id world
            in
                ( mid, updatedWorld )

        Nothing ->
            ( mid, world )
