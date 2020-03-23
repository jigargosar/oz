module Forest.DictZipper exposing (DictZipper, fromData)

import Dict exposing (Dict)
import ItemId exposing (ItemId)
import List.Extra
import Utils exposing (..)


type DictZipper a
    = DictZipper (IDDict a) (Data a) (List (Data a))


type alias IDDict a =
    Dict String (Data a)


add : Data a -> IDDict a -> IDDict a
add data =
    Dict.insert (ItemId.toString data.id) data


delete : ItemId -> IDDict a -> IDDict a
delete itemId =
    Dict.remove (ItemId.toString itemId)


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


right : DictZipper a -> Maybe (DictZipper a)
right (DictZipper dict data ancestors) =
    case nextSiblingsOf data dict of
        newData :: _ ->
            DictZipper (dict |> add data |> delete newData.id) newData ancestors
                |> Just

        _ ->
            Nothing
