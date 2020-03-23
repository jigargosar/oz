module Forest.DictZipper exposing (DictZipper, fromData)

import Dict exposing (Dict)
import ItemId exposing (ItemId)
import List.Extra
import Utils exposing (..)


type DictZipper a
    = DictZipper (IDDict a) (Data a) (List (Data a))


type alias IDDict a =
    Dict String (Data a)


type alias Data a =
    { a | id : ItemId, parentId : Maybe ItemId, idx : Int }


fromData : Data a -> DictZipper a
fromData data =
    DictZipper Dict.empty data []


nextSiblingsOf : Data a -> IDDict a -> List (Data a)
nextSiblingsOf data dict =
    Dict.values dict
        |> List.Extra.find (allPass [ .idx >> isGreaterThan data.idx, eqBy .parentId data ])
        |> List.sortBy .idx


childrenOf : ItemId -> IDDict a -> List (Data a)
childrenOf itemId dict =
    Dict.values dict
        |> List.Extra.find (allPass [ propEq .parentId (Just itemId) ])
        |> List.sortBy .idx


right : DictZipper a -> Maybe (DictZipper a)
right (DictZipper dict data ancestors) =
    case nextSiblingsOf data dict of
        newData :: _ ->
            DictZipper dict newData ancestors
                |> Just

        _ ->
            Nothing


up : DictZipper a -> Maybe (DictZipper a)
up (DictZipper dict _ cs) =
    case cs of
        first :: rest ->
            DictZipper dict first rest
                |> Just

        _ ->
            Nothing


down : DictZipper a -> Maybe (DictZipper a)
down (DictZipper dict c cs) =
    case childrenOf c.id dict of
        first :: _ ->
            DictZipper dict first (c :: cs)
                |> Just

        _ ->
            Nothing
