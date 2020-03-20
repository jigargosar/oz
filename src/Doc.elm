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



-- LINE DICT


type alias LineD =
    Dict String Line


addNew : Line -> LineD -> LineD
addNew line =
    ensureNotMember line >> Dict.insert (stringId line) line


replace : Line -> LineD -> LineD
replace line =
    ensureMember line >> Dict.insert (stringId line) line


ensureMember : Line -> LineD -> LineD
ensureMember line lineD =
    if Dict.member (stringId line) lineD then
        lineD

    else
        Debug.todo "replacing non existent line"


ensureNotMember : Line -> LineD -> LineD
ensureNotMember line lineD =
    if Dict.member (stringId line) lineD then
        Debug.todo "new line already exists"

    else
        lineD



-- MODEL


type Doc
    = Doc Model


type alias Model =
    { byId : Dict String Line
    , focusedId : ItemId
    }


focusedStringId : Model -> String
focusedStringId =
    .focusedId >> ItemId.toString


getFocused : Model -> Line
getFocused model =
    Dict.get (focusedStringId model) model.byId
        |> Maybe.withDefault (Debug.todo "focused line lost :(")


setFocused : Line -> Model -> Model
setFocused line model =
    if line.id == model.focusedId then
        { model | byId = replace line model.byId }

    else
        Debug.todo "setting focused with different id"


mapFocused : (Line -> Line) -> Model -> Model
mapFocused func model =
    setFocused (func (getFocused model)) model


new : Generator Doc
new =
    let
        newHelp : Line -> Model
        newHelp line =
            { byId = addNew line Dict.empty, focusedId = line.id }
    in
    newLine
        |> Random.map (newHelp >> Doc)


setTitle : String -> Doc
setTitle title =
    mapFocused (\l -> { l | title = title })
