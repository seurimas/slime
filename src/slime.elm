module Slime exposing (EntitySet, ComponentSpec, ComponentSet, EntitySetter, EntitySetter2, ComponentDeletor, initComponents, initIdSource, deletor, (&>), Entity, Entity2, Entity3, spawnEntity, spawnEntity2, setEntity, setEntity2, entities, entities2, getComponent, map, stepComponents, stepEntities, stepEntities2)

{-| This library provides an easy way to construct entity-component-system style
codeflow in Elm.

# Types
@docs EntitySet, ComponentSpec, ComponentSet, EntitySetter, EntitySetter2, EntitySetter3, ComponentDeletor, Entity, Entity2, Entity3

# Updates and Maps
@docs map, stepComponents, stepEntities, stepEntities2

# Initialization
@docs init

# Deletion
@docs deletor, (&>)

# Retrieval
@docs entities, entities2, getComponent

# Creation and setting
@docs setEntity, setEntity2, spawnEmpty, spawnEntity, spawnEntity2
-}

import Array exposing (Array, length, append, push, get, set, repeat, indexedMap, toList)
import Array.Extra exposing (zip, zip3, filterMap)
import Lazy.List exposing (LazyList, numbers, drop, (+++), headAndTail)


{-| -}
type alias ComponentSpec a rec =
    { getter : rec -> ComponentSet a
    , setter : rec -> ComponentSet a -> rec
    }


{-| -}
type alias ComponentDeletor rec =
    rec -> Int -> rec


{-| -}
type alias SystemStep rec =
    rec -> rec


{-| -}
type alias EntitySetter a rec =
    rec -> Entity a -> rec


{-| -}
type alias EntitySetter2 a b rec =
    rec -> Entity2 a b -> rec


{-| -}
type alias EntitySetter3 a b c rec =
    rec -> Entity3 a b c -> rec


{-| -}
type alias EntitySet rec =
    { rec
        | idSource : LazyList Int
    }


{-| -}
type alias Entity a =
    { id : Int
    , a : a
    }


{-| -}
type alias Entity2 a b =
    { id : Int
    , a : a
    , b : b
    }


{-| -}
type alias Entity3 a b c =
    { id : Int
    , a : a
    , b : b
    , c : c
    }


{-| -}
type alias ComponentSet a =
    { contents : Array (Maybe a)
    }


tag : Int -> Maybe a -> Maybe (Entity a)
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


tag2 : Int -> ( Maybe a, Maybe b ) -> Maybe (Entity2 a b)
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


tag3 : Int -> ( Maybe a, Maybe b, Maybe c ) -> Maybe (Entity3 a b c)
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


deleteComponent : Int -> ComponentSet a -> ComponentSet a
deleteComponent index components =
    let
        newContents =
            set index Nothing components.contents
    in
        { components | contents = newContents }


{-| -}
deletor : ComponentSpec a (EntitySet rec) -> ComponentDeletor (EntitySet rec)
deletor { getter, setter } record id =
    getter record
        |> deleteComponent id
        |> setter record


{-| -}
(&>) : ComponentDeletor rec -> ComponentDeletor rec -> ComponentDeletor rec
(&>) first second record index =
    first record index
        |> flip second index



{-
   Spawning and updating
-}


{-| -}
spawnEmpty : EntitySet rec -> ( EntitySet rec, Int )
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


spawnEntity : ComponentSpec a (EntitySet rec) -> EntitySet rec -> { a : a } -> ( EntitySet rec, Int )
spawnEntity { getter, setter } entitySet { a } =
    let
        ( updatedSet, id ) =
            spawnEmpty entitySet

        updatedComponents =
            getter updatedSet
                |> setComponent id a
    in
        ( setter updatedSet updatedComponents, id )


spawnEntity2 : ComponentSpec a (EntitySet rec) -> ComponentSpec b (EntitySet rec) -> EntitySet rec -> { a : a, b : b } -> ( EntitySet rec, Int )
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


setComponent : Int -> a -> ComponentSet a -> ComponentSet a
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


{-| Useful for time-based components which simply need to be updated incrementally
-}
stepComponents : (a -> a) -> ComponentSet a -> ComponentSet a
stepComponents update components =
    let
        newContents =
            Array.map (Maybe.map update) components.contents
    in
        { components | contents = newContents }


{-|
-}
stepEntities : ComponentSpec a rec -> (a -> a) -> rec -> rec
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
stepEntities2 : ComponentSpec a rec -> ComponentSpec b rec -> (Entity2 a b -> Entity2 a b) -> rec -> rec
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
stepEntities3 : ComponentSpec a rec -> ComponentSpec b rec -> ComponentSpec c rec -> (Entity3 a b c -> Entity3 a b c) -> rec -> rec
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
setEntity : ComponentSpec a rec -> EntitySetter a rec
setEntity { getter, setter } record entity =
    let
        updatedComponents =
            getter record
                |> setComponent entity.id entity.a
    in
        setter record updatedComponents


{-| -}
setEntity2 : ComponentSpec a rec -> ComponentSpec b rec -> EntitySetter2 a b rec
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
setEntity3 : ComponentSpec a rec -> ComponentSpec b rec -> ComponentSpec c rec -> EntitySetter3 a b c rec
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
getComponent : ComponentSet a -> Int -> Maybe a
getComponent components index =
    get index components.contents
        |> Maybe.andThen identity


completeEntities : Array (Maybe a) -> List a
completeEntities =
    (filterMap identity) >> toList


{-| -}
map : ComponentSpec a rec -> (a -> b) -> rec -> List b
map { getter } func record =
    getter record
        |> .contents
        |> filterMap identity
        |> toList
        |> List.map func


{-| -}
entities : ComponentSpec a rec -> rec -> List (Entity a)
entities { getter } record =
    getter record
        |> .contents
        |> indexedMap tag
        |> completeEntities


{-| -}
entities2 : ComponentSpec a rec -> ComponentSpec b rec -> rec -> List (Entity2 a b)
entities2 specA specB record =
    zip ((.contents << specA.getter) record) ((.contents << specB.getter) record)
        |> indexedMap tag2
        |> completeEntities


{-| -}
entities3 : ComponentSpec a rec -> ComponentSpec b rec -> ComponentSpec c rec -> rec -> List (Entity3 a b c)
entities3 specA specB specC record =
    zip3 ((.contents << specA.getter) record) ((.contents << specB.getter) record) ((.contents << specC.getter) record)
        |> indexedMap tag3
        |> completeEntities
