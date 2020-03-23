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


previousSiblingsOf : Data a -> IDDict a -> List (Data a)
previousSiblingsOf data dict =
    Dict.values dict
        |> List.Extra.find (allPass [ .idx >> isLessThanOrEq data.idx, eqBy .parentId data ])
        |> List.sortBy .idx


siblingsZipperOf : Data a -> IDDict a -> ( List (Data a), Data a, List (Data a) )
siblingsZipperOf c dict =
    ( previousSiblingsOf c dict, c, nextSiblingsOf c dict )


withParentId : Maybe ItemId -> IDDict a -> List (Data a)
withParentId parentId dict =
    Dict.values dict
        |> List.Extra.find (allPass [ propEq .parentId parentId ])
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



--insertRightGo newData (DictZipper dict c cs) =
--    let
--         updatedData =
--             {newData| parentId = c.parentId}
--
--         (l,ac, r) = siblingsZipperOf c dict
--
--         newSiblings = (l++ ac :: r)
--            |> List.indexedMap (\idx d -> {d| idx = idx})
--
--
--
--    in
--
--
