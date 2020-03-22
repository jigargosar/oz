module OutlineDict exposing (OutlineDict)

import Dict exposing (Dict)
import ItemId exposing (ItemId)
import Tree.Zipper exposing (Zipper)


type OutlineDict
    = OutlineDict (Dict String ( Maybe ItemId, Line ))


type alias Line =
    { id : ItemId
    , title : String
    }


type OutlineFZ
    = OutlineFZ (Zipper Line)
