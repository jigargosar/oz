module Doc exposing (Doc)

import Dict exposing (Dict)
import ItemId exposing (ItemId)
import Random exposing (Generator)


type Doc
    = Doc Model


type alias Line =
    { id : ItemId
    , title : String
    }


newLine : Generator Line
newLine =
    ItemId.generator
        |> Random.map (\id -> Line id "")


type alias Model =
    { byId : Dict String Line
    , focusedId : ItemId
    }
