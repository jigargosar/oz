module OutlineDict exposing (OutlineDict)

import Dict exposing (Dict)
import ItemId exposing (ItemId)


type OutlineDict
    = OutlineDict Model


type alias Model =
    { dict : Dict String Line }


type alias Line =
    { id : ItemId
    , title : String
    , parentId : Maybe ItemId
    }
