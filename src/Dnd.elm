module Dnd exposing
    ( Beacon
    , Pointer
    , XY
    , beaconAttr
    , beaconDecoder
    , clientXYDecoder
    , dndClosestCandidateLocation
    , dndDraggedXY
    , dragEvents
    , setClientXY
    )

import Html exposing (Attribute)
import Html.Attributes exposing (attribute, draggable)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import OutlineDoc as Doc exposing (CandidateLocation)


type alias Pointer =
    { clientXY : XY
    , offsetXY : XY
    }


type alias Beacon =
    ( CandidateLocation, Rect )


setClientXY : XY -> Pointer -> Pointer
setClientXY clientXY pointer =
    { pointer | clientXY = clientXY }


dndDraggedXY : Pointer -> XY
dndDraggedXY dnd =
    subtractXY dnd.clientXY dnd.offsetXY


dndClosestCandidateLocation : List Beacon -> Pointer -> Maybe CandidateLocation
dndClosestCandidateLocation beacons dnd =
    let
        draggedXY : XY
        draggedXY =
            dndDraggedXY dnd

        sortByFunc : Beacon -> Float
        sortByFunc ( _, rect ) =
            distanceFromRectCenterTo draggedXY rect
    in
    beacons
        |> List.sortBy sortByFunc
        |> List.head
        |> Maybe.map Tuple.first


beaconDecoder : Decoder Beacon
beaconDecoder =
    JD.map2 Tuple.pair
        (JD.field "id" Doc.candidateLocationDecoder)
        rectDecoder


dataBeacon : Value -> Attribute msg
dataBeacon value =
    attribute "data-beacon" (JE.encode 0 value)


beaconAttr : CandidateLocation -> Attribute msg
beaconAttr candidateLocation =
    dataBeacon (Doc.candidateLocationEncoder candidateLocation)


dragEvents : (Pointer -> msg) -> List (Html.Attribute msg)
dragEvents onDragStart =
    [ draggable "true"
    , preventDefaultOn "dragstart"
        (JD.map2 (\clientXY offsetXY -> onDragStart (Pointer clientXY offsetXY))
            clientXYDecoder
            offsetXYDecoder
            |> preventDefault True
        )
    ]


preventDefault : Bool -> Decoder b -> Decoder ( b, Bool )
preventDefault bool =
    JD.map (\msg -> ( msg, bool ))


clientXYDecoder : Decoder XY
clientXYDecoder =
    JD.map2 XY
        (JD.field "clientX" JD.float)
        (JD.field "clientY" JD.float)


offsetXYDecoder : Decoder XY
offsetXYDecoder =
    JD.map2 XY
        (JD.field "offsetX" JD.float)
        (JD.field "offsetY" JD.float)



-- XY


type alias XY =
    { x : Float, y : Float }


subtractXY : XY -> XY -> XY
subtractXY a b =
    XY (a.x - b.x) (a.y - b.y)



-- RECT


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


rectDecoder : Decoder Rect
rectDecoder =
    JD.map4 Rect
        (JD.field "x" JD.float)
        (JD.field "y" JD.float)
        (JD.field "width" JD.float)
        (JD.field "height" JD.float)


rectCenter : Rect -> XY
rectCenter rect =
    XY (rect.x + (rect.width / 2)) (rect.y + (rect.height / 2))


distanceFromRectCenterTo : XY -> Rect -> Float
distanceFromRectCenterTo xy rect =
    let
        fromXY =
            rectCenter rect
    in
    sqrt (((xy.x - fromXY.x) ^ 2) + ((xy.y - fromXY.y) ^ 2))
