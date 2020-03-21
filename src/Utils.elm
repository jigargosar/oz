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


allPass : List (b -> Bool) -> b -> Bool
allPass fs val =
    List.all ((|>) val) fs


cond : List ( a -> Bool, a -> b ) -> a -> Maybe b
cond conditions a =
    List.Extra.find (Tuple.first >> apply a) conditions
        |> Maybe.map (Tuple.second >> apply a)


condAlways : List ( b -> Bool, a ) -> b -> Maybe a
condAlways conditions =
    cond (List.map (Tuple.mapSecond always) conditions)


apply : a -> (a -> b) -> b
apply =
    (|>)


effect func ( m, c ) =
    ( m, Cmd.batch [ c, func m ] )


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
