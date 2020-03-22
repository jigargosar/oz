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


type OutlineZipper
    = OutlineZipper (Zipper Line)


type Error
    = Error


validateOZ : OutlineZipper -> Result Error OutlineZipper
validateOZ =
    Debug.todo "impl"


validateOD : OutlineDict -> Result Error OutlineDict
validateOD =
    Debug.todo "impl"
