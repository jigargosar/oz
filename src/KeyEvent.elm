module KeyEvent exposing (..)

import Json.Decode as JD exposing (Decoder)
import Utils exposing (..)


type alias KeyEvent =
    { key : String
    , ctrl : Bool
    , shift : Bool
    , alt : Bool
    , meta : Bool
    , targetTagName : String
    }


decoder : Decoder KeyEvent
decoder =
    JD.succeed KeyEvent
        |> requiredString "key"
        |> requiredBool "ctrlKey"
        |> requiredBool "shiftKey"
        |> requiredBool "altKey"
        |> requiredBool "metaKey"
        |> requiredAt [ "target", "tagName" ] JD.string


hot : String -> KeyEvent -> Bool
hot name ke =
    ke.key == name && not (ke.ctrl || ke.shift || ke.alt || ke.meta)


ctrl : String -> KeyEvent -> Bool
ctrl name ke =
    ke.key == name && ke.ctrl && not (ke.shift || ke.alt || ke.meta)


shift : String -> KeyEvent -> Bool
shift name ke =
    ke.key == name && ke.shift && not (ke.ctrl || ke.alt || ke.meta)


targetInputOrButton : KeyEvent -> Bool
targetInputOrButton ke =
    List.member ke.targetTagName [ "INPUT", "BUTTON" ]
