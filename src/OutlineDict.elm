module OutlineDict exposing (OutlineDict)

import Dict exposing (Dict)
import ItemId exposing (ItemId)
import Tree.Zipper exposing (Zipper)


type Valid
    = Valid


type OutlineDict valid
    = OutlineDict (Dict String ( Maybe ItemId, Line ))


type alias Line =
    { id : ItemId
    , title : String
    }


type OutlineZipper valid
    = OutlineZipper (Zipper Line)


type Error
    = Error


validateOZ : OutlineZipper a -> Result Error (OutlineZipper Valid)
validateOZ =
    Debug.todo "impl"


validateOD : OutlineDict a -> Result Error (OutlineDict Valid)
validateOD =
    Debug.todo "impl"


toDict : OutlineZipper Valid -> OutlineDict Valid
toDict =
    Debug.todo "impl"


fromDict : OutlineDict Valid -> OutlineZipper Valid
fromDict =
    Debug.todo "impl"
