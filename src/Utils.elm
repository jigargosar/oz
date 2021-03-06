module Utils exposing (..)

import Html
import Html.Attributes
import Json.Decode as JD exposing (Decoder)
import List.Extra
import Maybe.Extra


eqBy func a b =
    func a == func b


neqBy func a b =
    func a /= func b


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required name decoder_ =
    JD.map2 (|>) (JD.field name decoder_)


requiredAt : List String -> Decoder a -> Decoder (a -> b) -> Decoder b
requiredAt path decoder_ =
    JD.map2 (|>) (JD.at path decoder_)


requiredBool name =
    required name JD.bool


requiredString name =
    required name JD.string


requiredList name liDecoder =
    required name (JD.list liDecoder)


allPass : List (b -> Bool) -> b -> Bool
allPass fs val =
    List.all ((|>) val) fs


anyPass : List (b -> Bool) -> b -> Bool
anyPass fs val =
    List.any ((|>) val) fs


cond : List ( a -> Bool, a -> b ) -> a -> Maybe b
cond conditions a =
    List.Extra.find (Tuple.first >> apply a) conditions
        |> Maybe.map (Tuple.second >> apply a)


condAlways : List ( b -> Bool, a ) -> b -> Maybe a
condAlways conditions =
    cond (List.map (Tuple.mapSecond always) conditions)


cmdIf bool cmd =
    if bool then
        cmd

    else
        Cmd.none


apply : a -> (a -> b) -> b
apply =
    (|>)


effect func ( m, c ) =
    ( m, Cmd.batch [ c, func m ] )


firstOf : List (a -> Maybe b) -> a -> Maybe b
firstOf =
    Maybe.Extra.oneOf


nonBlank : String -> Maybe String
nonBlank =
    String.trim
        >> (\trimmedString ->
                if trimmedString == "" then
                    Nothing

                else
                    Just trimmedString
           )


ignoreNothing : (b -> Maybe b) -> b -> b
ignoreNothing f v =
    f v |> Maybe.withDefault v


applyWhileJust : (a -> Maybe a) -> a -> a
applyWhileJust func a =
    case func a of
        Just a2 ->
            applyWhileJust func a2

        Nothing ->
            a


findWithIterator : (a -> Bool) -> (a -> Maybe a) -> a -> Maybe a
findWithIterator pred iterator zipper =
    if pred zipper then
        Just zipper

    else
        case iterator zipper of
            Just nextAcc ->
                findWithIterator pred iterator nextAcc

            Nothing ->
                Nothing


idEq : b -> { a | id : b } -> Bool
idEq =
    propEq .id


propEq : (c -> b) -> b -> c -> Bool
propEq func val obj =
    func obj == val


htmlMaybe : (a -> Html.Html msg) -> Maybe a -> Html.Html msg
htmlMaybe func =
    Maybe.map func >> Maybe.withDefault (Html.text "")


classIf bool classValue =
    if bool then
        Html.Attributes.class classValue

    else
        Html.Attributes.class ""


attrIf bool attrFunc attrValue =
    if bool then
        attrFunc attrValue

    else
        Html.Attributes.class ""


swap ( a, b ) =
    ( b, a )


isBlank =
    String.trim >> String.isEmpty


noHtml =
    Html.text ""


neq =
    (/=)


eq =
    (==)


add =
    (+)


sub =
    (-)


mul =
    (*)


idiv =
    (//)


fdiv =
    (/)


pow =
    (^)


cons =
    (::)


flip func a b =
    func b a


isGreaterThan target src =
    src > target


isLessThanOrEq target src =
    src <= target


uncurry f ( a, b ) =
    f a b


eqById a b =
    a.id == b.id


findX : (a -> Maybe a) -> (a -> Bool) -> a -> Maybe a
findX nextValFunc pred val =
    case nextValFunc val of
        Just nod ->
            if pred nod then
                Just nod

            else
                findX nextValFunc pred nod

        Nothing ->
            Nothing


findI : (a -> Maybe a) -> (a -> Bool) -> a -> Maybe a
findI nextValFunc pred val =
    if pred val then
        Just val

    else
        findX nextValFunc pred val
