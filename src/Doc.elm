module Doc exposing (Doc, new)

import Dict exposing (Dict)
import ItemId exposing (ItemId)
import Random exposing (Generator)



-- LINE


type alias Line =
    { id : ItemId
    , title : String
    }


stringId : Line -> String
stringId =
    .id >> ItemId.toString


newLine : Generator Line
newLine =
    ItemId.generator
        |> Random.map (\id -> Line id "")



-- MODEL


type Doc
    = Doc Model


type alias Model =
    { byId : Dict String Line
    , focusedId : ItemId
    }


type alias LineD =
    Dict String Line


insertNew : Line -> LineD -> LineD
insertNew line =
    ensureNotMember line >> Dict.insert (stringId line) line


ensureNotMember : Line -> LineD -> LineD
ensureNotMember line lineD =
    if Dict.member (stringId line) lineD then
        Debug.todo "new line already exists"

    else
        lineD


new : Generator Doc
new =
    let
        newHelp : Line -> Model
        newHelp line =
            { byId = insertNew line Dict.empty, focusedId = line.id }
    in
    newLine
        |> Random.map (newHelp >> Doc)
