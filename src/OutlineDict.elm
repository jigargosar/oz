module OutlineDict exposing (OutlineDict)

import Dict exposing (Dict)
import ItemId exposing (ItemId)
import Tree.Zipper exposing (Zipper)


type OutlineDict
    = OutlineDict ODM


type alias ODM =
    { current : ( Maybe ItemId, Line )
    , dict : Dict String ( Maybe ItemId, Line )
    }


type alias Line =
    { id : ItemId
    , title : String
    }


type OutlineZipper
    = OutlineZipper OZM


type alias OZM =
    Zipper Line


type Error
    = Error


fromOZM : OZM -> Result Error OutlineZipper
fromOZM =
    Debug.todo "impl"


validateODM : ODM -> Result Error OutlineDict
validateODM =
    Debug.todo "impl"
