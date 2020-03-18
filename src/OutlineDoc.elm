module OutlineDoc exposing
    ( CandidateLocation
    , OutlineDoc
    , addNew
    , after
    , ancestorIds
    , appendIn
    , appendInPreviousSibling
    , before
    , candidateLocationDecoder
    , candidateLocationEncoder
    , collapse
    , currentId
    , currentTitle
    , decoder
    , encoder
    , expand
    , goBackward
    , goForward
    , moveAfterNextSiblingOrPrependInNextSiblingOfParent
    , moveAfterParent
    , moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent
    , moveCurrentToCandidateLocation
    , moveCursorToItemId
    , prependIn
    , removeIfBlankLeaf
    , restructureCurrentNode
    , restructureWithContext
    , setTitleUnlessBlank
    )

import ItemForestZipper as FIZ exposing (FIZ, Location(..))
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Maybe.Extra
import Random exposing (Generator)



-- CANDIDATE LOCATION


type CandidateLocation
    = CandidateLocation FIZ.Location ItemId


candidateLocationEncoder : CandidateLocation -> Value
candidateLocationEncoder (CandidateLocation loc itemId) =
    let
        encodeHelp : String -> Value
        encodeHelp tagName =
            JE.object
                [ ( "tag", JE.string tagName )
                , ( "id", ItemId.itemIdEncoder itemId )
                ]
    in
    case loc of
        Before ->
            encodeHelp "Before"

        After ->
            encodeHelp "After"

        PrependChild ->
            encodeHelp "PrependChild"

        AppendChild ->
            encodeHelp "AppendChild"


candidateLocationDecoder : Decoder CandidateLocation
candidateLocationDecoder =
    let
        decodeHelp : Location -> Decoder CandidateLocation
        decodeHelp tag =
            JD.field "id" ItemId.itemIdDecoder
                |> JD.map (CandidateLocation tag)

        tagDecoder : String -> Decoder CandidateLocation
        tagDecoder tag =
            case tag of
                "Before" ->
                    decodeHelp Before

                "After" ->
                    decodeHelp After

                "PrependChild" ->
                    decodeHelp PrependChild

                "AppendChild" ->
                    decodeHelp AppendChild

                _ ->
                    JD.fail ("unknown tag for CandidateLocation: " ++ tag)
    in
    JD.field "tag" JD.string |> JD.andThen tagDecoder


before : ItemId -> CandidateLocation
before =
    CandidateLocation Before


after : ItemId -> CandidateLocation
after =
    CandidateLocation After


prependIn : ItemId -> CandidateLocation
prependIn =
    CandidateLocation PrependChild


appendIn : ItemId -> CandidateLocation
appendIn =
    CandidateLocation AppendChild



-- ITEM


type alias Item =
    FIZ.Item



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


addNew : OutlineDoc -> Generator OutlineDoc
addNew =
    unwrap >> FIZ.addNew >> Random.map OutlineDoc


moveCursorToItemId : ItemId -> OutlineDoc -> Maybe OutlineDoc
moveCursorToItemId itemId =
    mapMaybe (FIZ.gotoId itemId)


setTitleUnlessBlank : String -> OutlineDoc -> OutlineDoc
setTitleUnlessBlank title =
    map
        (FIZ.setTitle title |> ignoreNothing)


ignoreNothing f v =
    f v |> Maybe.withDefault v


expand : OutlineDoc -> Maybe OutlineDoc
expand =
    mapMaybe FIZ.expand


collapse : OutlineDoc -> Maybe OutlineDoc
collapse =
    mapMaybe FIZ.collapse


removeIfBlankLeaf : OutlineDoc -> OutlineDoc
removeIfBlankLeaf =
    map
        (FIZ.deleteEmpty |> ignoreNothing)


currentTitle : OutlineDoc -> String
currentTitle =
    unwrap >> FIZ.getTitle


ancestorIds =
    unwrap >> FIZ.ancestorIds


currentId : OutlineDoc -> ItemId
currentId =
    unwrap >> FIZ.getId


relocateBy : FIZ.Location -> (FIZ -> Maybe FIZ) -> OutlineDoc -> Maybe OutlineDoc
relocateBy a b =
    mapMaybe (FIZ.relocateBy a b)


moveAfterParent : OutlineDoc -> Maybe OutlineDoc
moveAfterParent =
    relocateBy FIZ.After FIZ.goUp


appendInPreviousSibling : OutlineDoc -> Maybe OutlineDoc
appendInPreviousSibling =
    relocateBy FIZ.AppendChild FIZ.goLeft


moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent : OutlineDoc -> Maybe OutlineDoc
moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent =
    Maybe.Extra.oneOf
        [ relocateBy FIZ.Before FIZ.goLeft
        , relocateBy FIZ.AppendChild (FIZ.goUp >> Maybe.andThen FIZ.goLeft)
        ]


moveAfterNextSiblingOrPrependInNextSiblingOfParent : OutlineDoc -> Maybe OutlineDoc
moveAfterNextSiblingOrPrependInNextSiblingOfParent =
    Maybe.Extra.oneOf
        [ relocateBy FIZ.After FIZ.goRight
        , relocateBy FIZ.PrependChild (FIZ.goUp >> Maybe.andThen FIZ.goRight)
        ]


moveCurrentToCandidateLocation : CandidateLocation -> OutlineDoc -> Maybe OutlineDoc
moveCurrentToCandidateLocation (CandidateLocation loc itemId) =
    mapMaybe (FIZ.relocate loc itemId)


restructureWithContext render =
    unwrap >> FIZ.restructureWithContext render


restructureCurrentNode render =
    unwrap >> FIZ.restructureCursorWithContext render


goBackward : OutlineDoc -> Maybe OutlineDoc
goBackward =
    mapMaybe FIZ.goBackward


goForward : OutlineDoc -> Maybe OutlineDoc
goForward =
    mapMaybe FIZ.goForward



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
