module OutlineDoc exposing
    ( CandidateLocation(..)
    , Item
    , ItemId
    , OutlineDoc
    , appendInPreviousSibling
    , candidateLocationDecoder
    , candidateLocationEncoder
    , currentId
    , currentTitle
    , decoder
    , encoder
    , goBackward
    , goForward
    , hasVisibleChildren
    , insertNewAfter
    , moveAfterNextSiblingOrPrependInNextSiblingOfParent
    , moveAfterParent
    , moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent
    , moveCurrentToCandidateLocation
    , moveFocusToItemId
    , prependNewChild
    , removeIfBlankLeaf
    , restructure
    , restructureFocused
    , setTitleUnlessBlank
    )

import ItemForestZipper as FIZ exposing (FIZ)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Maybe.Extra
import Random exposing (Generator)



-- CANDIDATE LOCATION


type CandidateLocation
    = Before ItemId
    | After ItemId
    | PrependIn ItemId
    | AppendIn ItemId


candidateLocationEncoder : CandidateLocation -> Value
candidateLocationEncoder candidateLocation =
    let
        encodeHelp : String -> ItemId -> Value
        encodeHelp tagName itemId =
            JE.object
                [ ( "tag", JE.string tagName )
                , ( "id", FIZ.itemIdEncoder itemId )
                ]
    in
    case candidateLocation of
        Before itemId ->
            encodeHelp "Before" itemId

        After itemId ->
            encodeHelp "After" itemId

        PrependIn itemId ->
            encodeHelp "PrependIn" itemId

        AppendIn itemId ->
            encodeHelp "AppendIn" itemId


candidateLocationDecoder : Decoder CandidateLocation
candidateLocationDecoder =
    let
        decodeHelp : (ItemId -> CandidateLocation) -> Decoder CandidateLocation
        decodeHelp tag =
            JD.field "id" FIZ.itemIdDecoder
                |> JD.map tag

        tagDecoder : String -> Decoder CandidateLocation
        tagDecoder tag =
            case tag of
                "Before" ->
                    decodeHelp Before

                "After" ->
                    decodeHelp After

                "PrependIn" ->
                    decodeHelp PrependIn

                "AppendIn" ->
                    decodeHelp AppendIn

                _ ->
                    JD.fail ("unknown tag for CandidateLocation: " ++ tag)
    in
    JD.field "tag" JD.string |> JD.andThen tagDecoder



-- ITEM


type alias Item =
    FIZ.Item


type alias ItemId =
    FIZ.ItemId



-- DOC MODEL


type OutlineDoc
    = OutlineDoc FIZ


encoder : OutlineDoc -> Value
encoder (OutlineDoc zipper) =
    FIZ.encoder zipper


decoder : Decoder OutlineDoc
decoder =
    FIZ.decoder |> JD.map OutlineDoc


map : (FIZ -> FIZ) -> OutlineDoc -> OutlineDoc
map func (OutlineDoc z) =
    func z |> OutlineDoc


mapMaybe : (FIZ -> Maybe FIZ) -> OutlineDoc -> Maybe OutlineDoc
mapMaybe func (OutlineDoc z) =
    func z |> Maybe.map OutlineDoc


unwrap : OutlineDoc -> FIZ
unwrap (OutlineDoc z) =
    z



-- NEW INSERTIONS


prependNewChild : OutlineDoc -> Generator OutlineDoc
prependNewChild =
    insertNewHelp FIZ.newChild


insertNewAfter : OutlineDoc -> Generator OutlineDoc
insertNewAfter =
    insertNewHelp FIZ.newSibling


insertNewHelp insertFunc (OutlineDoc z) =
    insertFunc z |> Random.map OutlineDoc


moveFocusToItemId : ItemId -> OutlineDoc -> Maybe OutlineDoc
moveFocusToItemId itemId =
    mapMaybe (FIZ.gotoId itemId)


setTitleUnlessBlank : String -> OutlineDoc -> OutlineDoc
setTitleUnlessBlank title =
    map
        (FIZ.setTitle title |> ignoreNothing)


ignoreNothing f v =
    f v |> Maybe.withDefault v


removeIfBlankLeaf : OutlineDoc -> OutlineDoc
removeIfBlankLeaf =
    map
        (FIZ.deleteEmpty |> ignoreNothing)


currentTitle : OutlineDoc -> String
currentTitle =
    unwrap >> FIZ.getTitle


currentId : OutlineDoc -> ItemId
currentId =
    unwrap >> FIZ.getId


relocateFocusedBy : FIZ.Location -> (FIZ -> Maybe FIZ) -> OutlineDoc -> Maybe OutlineDoc
relocateFocusedBy a b =
    mapMaybe (FIZ.relocateBy a b)


moveAfterParent : OutlineDoc -> Maybe OutlineDoc
moveAfterParent =
    relocateFocusedBy FIZ.After FIZ.goUp


appendInPreviousSibling : OutlineDoc -> Maybe OutlineDoc
appendInPreviousSibling =
    relocateFocusedBy FIZ.AppendChild FIZ.goLeft


moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent : OutlineDoc -> Maybe OutlineDoc
moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent =
    Maybe.Extra.oneOf
        [ relocateFocusedBy FIZ.Before FIZ.goLeft
        , relocateFocusedBy FIZ.AppendChild (FIZ.goUp >> Maybe.andThen FIZ.goLeft)
        ]


moveAfterNextSiblingOrPrependInNextSiblingOfParent : OutlineDoc -> Maybe OutlineDoc
moveAfterNextSiblingOrPrependInNextSiblingOfParent =
    Maybe.Extra.oneOf
        [ relocateFocusedBy FIZ.After FIZ.goRight
        , relocateFocusedBy FIZ.PrependChild (FIZ.goUp >> Maybe.andThen FIZ.goRight)
        ]


moveCurrentToCandidateLocation : CandidateLocation -> OutlineDoc -> Maybe OutlineDoc
moveCurrentToCandidateLocation cl =
    mapMaybe
        (case cl of
            Before itemId ->
                FIZ.relocate FIZ.Before itemId

            After itemId ->
                FIZ.relocate FIZ.After itemId

            PrependIn itemId ->
                FIZ.relocate FIZ.PrependChild itemId

            AppendIn itemId ->
                FIZ.relocate FIZ.AppendChild itemId
        )


restructure : (Item -> List c -> c) -> OutlineDoc -> List c
restructure render =
    unwrap >> FIZ.restructure render


restructureFocused : (Item -> List c -> c) -> OutlineDoc -> c
restructureFocused render =
    unwrap >> FIZ.restructureNodeAtCursor render


goBackward : OutlineDoc -> Maybe OutlineDoc
goBackward =
    mapMaybe FIZ.goBackward


goForward : OutlineDoc -> Maybe OutlineDoc
goForward =
    mapMaybe FIZ.goForward


hasVisibleChildren : OutlineDoc -> Bool
hasVisibleChildren =
    unwrap >> FIZ.hasVisibleChildren



-- ForestZipper Extra
{-

   initialItemGenerator : Generator (List Item)
   initialItemGenerator =
               [ "Quick Brown Fox Jumped Over The Lazy Dog"
               , "Take Notes"
               , "Thou shall not experiment with experiments"
               , "Watch Movies"
               , "Run the mill"
               ]
                   |> List.map OutlineDoc.itemGenerator
                   |> Random.Extra.combine
-}
