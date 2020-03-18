module Utils exposing (..)

import Json.Decode as JD exposing (Decoder)
import List.Extra


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
