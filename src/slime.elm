module Slime exposing (EntityID, EntitySet, ComponentSpec, ComponentSet, EntitySetter, EntitySetter2, EntityDeletor, initComponents, initIdSource, deleteEntity, (&->), Entity, Entity2, Entity3, spawnEntity, spawnEntity2, setEntity, setEntity2, entities, entities2, getComponent, map, stepEntities, stepEntities2)

{-| This library provides an easy way to construct entity-component-system style
codeflow in Elm.

# Types
@docs EntityID, EntitySet, ComponentSpec, ComponentSet, EntitySetter, EntitySetter2, EntitySetter3, EntityDeletor, Entity, Entity2, Entity3

# Updates and Maps
@docs map, stepEntities, stepEntities2

# Initialization
@docs init

# Deletion
@docs deleteEntity, (&->)

# Retrieval
@docs entities, entities2, getComponent

# Creation and setting
@docs setEntity, setEntity2, spawnEmpty, spawnEntity, spawnEntity2
-}

import Array exposing (Array, length, append, push, get, set, repeat, indexedMap, toList)
import Array.Extra exposing (zip, zip3, filterMap)
import Lazy.List exposing (LazyList, numbers, drop, (+++), headAndTail)


{-| -}
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
type alias SystemStep world =
    world -> world


{-| -}
type alias EntitySetter a world =
    world -> Entity a -> world


{-| -}
type alias EntitySetter2 a b world =
    world -> Entity2 a b -> world


{-| -}
type alias EntitySetter3 a b c world =
    world -> Entity3 a b c -> world


{-| -}
type alias EntitySet world =
    { world
        | idSource : LazyList EntityID
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


{-| -}
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


initComponents : ComponentSet a
initComponents =
    { contents = Array.empty }


initIdSource =
    numbers



{-
   Deletion
-}


deleteComponent : EntityID -> ComponentSet a -> ComponentSet a
deleteComponent index components =
    let
        newContents =
            set index Nothing components.contents
    in
        { components | contents = newContents }


{-| -}
deleteEntity : ComponentSpec a (EntitySet world) -> EntityDeletor (EntitySet world)
deleteEntity { getter, setter } record id =
    getter record
        |> deleteComponent id
        |> setter record


{-| -}
(&->) : EntityDeletor (EntitySet world) -> ComponentSpec b (EntitySet world) -> EntityDeletor (EntitySet world)
(&->) accumulatedDelete newSpec record index =
    accumulatedDelete record index
        |> flip (deleteEntity newSpec) index
infixl 1 &->



{-
   Spawning and updating
-}


{-| -}
spawnEmpty : EntitySet world -> ( EntitySet world, EntityID )
spawnEmpty entitySet =
    let
        alwaysPresent =
            headAndTail entitySet.idSource
    in
        case alwaysPresent of
            Just ( nextId, remaining ) ->
                ( { entitySet | idSource = remaining }, nextId )

            Nothing ->
                ( entitySet, 0 )


spawnEntity : ComponentSpec a (EntitySet world) -> EntitySet world -> { a : a } -> ( EntitySet world, EntityID )
spawnEntity { getter, setter } entitySet { a } =
    let
        ( updatedSet, id ) =
            spawnEmpty entitySet

        updatedComponents =
            getter updatedSet
                |> setComponent id a
    in
        ( setter updatedSet updatedComponents, id )


spawnEntity2 : ComponentSpec a (EntitySet world) -> ComponentSpec b (EntitySet world) -> EntitySet world -> { a : a, b : b } -> ( EntitySet world, EntityID )
spawnEntity2 specA specB entitySet { a, b } =
    let
        ( updatedSet, id ) =
            spawnEmpty entitySet

        updatedComponentsA =
            specA.getter updatedSet
                |> setComponent id a

        updatedComponentsB =
            specB.getter updatedSet
                |> setComponent id b
    in
        ( specA.setter updatedSet updatedComponentsA |> flip specB.setter updatedComponentsB, id )


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


{-|
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


{-|
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


{-|
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


{-| -}
setEntity : ComponentSpec a world -> EntitySetter a world
setEntity { getter, setter } record entity =
    let
        updatedComponents =
            getter record
                |> setComponent entity.id entity.a
    in
        setter record updatedComponents


{-| -}
setEntity2 : ComponentSpec a world -> ComponentSpec b world -> EntitySetter2 a b world
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


{-| -}
setEntity3 : ComponentSpec a world -> ComponentSpec b world -> ComponentSpec c world -> EntitySetter3 a b c world
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


{-| -}
getComponent : ComponentSet a -> EntityID -> Maybe a
getComponent components index =
    get index components.contents
        |> Maybe.andThen identity


completeEntities : Array (Maybe a) -> List a
completeEntities =
    (filterMap identity) >> toList


{-| -}
map : ComponentSpec a world -> (a -> b) -> world -> List b
map { getter } func record =
    getter record
        |> .contents
        |> filterMap identity
        |> toList
        |> List.map func


{-| -}
entities : ComponentSpec a world -> world -> List (Entity a)
entities { getter } record =
    getter record
        |> .contents
        |> indexedMap tag
        |> completeEntities


{-| -}
entities2 : ComponentSpec a world -> ComponentSpec b world -> world -> List (Entity2 a b)
entities2 specA specB record =
    zip ((.contents << specA.getter) record) ((.contents << specB.getter) record)
        |> indexedMap tag2
        |> completeEntities


{-| -}
entities3 : ComponentSpec a world -> ComponentSpec b world -> ComponentSpec c world -> world -> List (Entity3 a b c)
entities3 specA specB specC record =
    zip3 ((.contents << specA.getter) record) ((.contents << specB.getter) record) ((.contents << specC.getter) record)
        |> indexedMap tag3
        |> completeEntities
