module Slime exposing (EntitySet, ComponentSpec, EntitySetter, EntitySetter2, ComponentDeletor, deletor, (&&), Entity, Entity2, Entity3, spawnEntity, setEntity, setEntity2, entities, entities2, getComponent)

{-| This library provides an easy way to construct entity-component-system style
codeflow in Elm.

# Types
@docs EntitySet, ComponentSpec, EntitySetter, EntitySetter2, ComponentDeletor, Entity, Entity2, Entity3

# Deletion
@docs deletor, (&&)

# Retrieval
@docs entities, entities2, getComponent

# Creation and setting
@docs setEntity, setEntity2, spawnEntity
-}

import Array exposing (Array, length, append, push, get, set, repeat, indexedMap, toList)
import Array.Extra exposing (zip, filterMap)
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
type alias EntitySetter a rec =
    rec -> Entity a -> rec


{-| -}
type alias EntitySetter2 a b rec =
    rec -> Entity2 a b -> rec


{-| -}
type alias EntitySet rec =
    { rec
        | deletors : List (ComponentDeletor rec)
        , idSource : LazyList Int
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


addB : b -> Entity a -> Entity2 a b
addB b e =
    { id = e.id
    , a = e.a
    , b = b
    }


{-| -}
type alias Entity3 a b c =
    { id : Int
    , a : a
    , b : b
    , c : c
    }


addC : c -> Entity2 a b -> Entity3 a b c
addC c e =
    { id = e.id
    , a = e.a
    , b = e.b
    , c = c
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


tag2 : Int -> ( Maybe a, Maybe b ) -> Maybe (Entity2 a b)
tag2 i ( ma, mb ) =
    tag i ma
        |> Maybe.map2 addB mb


tag3 : Int -> ( Maybe a, Maybe b, Maybe c ) -> Maybe (Entity3 a b c)
tag3 i ( ma, mb, mc ) =
    tag2 i ( ma, mb )
        |> Maybe.map2 addC mc



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
deletor : ComponentSpec a rec -> ComponentDeletor rec
deletor { getter, setter } record id =
    getter record
        |> deleteComponent id
        |> setter record


{-| -}
(&&) : ComponentDeletor rec -> ComponentDeletor rec -> ComponentDeletor rec
(&&) first second record index =
    first record index
        |> flip second index



{-
   Spawning and updating
-}


{-| -}
spawnEntity : EntitySet rec -> ( EntitySet rec, Int )
spawnEntity entitySet =
    let
        alwaysPresent =
            headAndTail entitySet.idSource
    in
        case alwaysPresent of
            Just ( nextId, remaining ) ->
                ( { entitySet | idSource = remaining }, nextId )

            Nothing ->
                ( entitySet, 0 )


setComponent : Int -> a -> ComponentSet a -> ComponentSet a
setComponent index value components =
    let
        missing =
            index - length components.contents

        newContents =
            append components.contents (repeat missing Nothing)
                |> push (Just value)
    in
        { components | contents = newContents }


{-| Useful for time-based components which simply need to be updated incrementally
-}
stepComponents : (a -> a) -> ComponentSet a -> ComponentSet a
stepComponents update components =
    let
        newContents =
            Array.map (Maybe.map update) components.contents
    in
        { components | contents = newContents }


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
