module OutlineDoc exposing
    ( CandidateLocation
    , OutlineDoc
    , addNew
    , after
    , ancestorIds
    , appendIn
    , before
    , candidateLocationDecoder
    , candidateLocationEncoder
    , collapse
    , collapseOrNavParent
    , currentId
    , currentTitle
    , decoder
    , encoder
    , expand
    , expandOrGoForward
    , goBackward
    , goForward
    , gotoId
    , indent
    , moveAfterNextSiblingOrPrependInNextSiblingOfParent
    , moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent
    , new
    , prependIn
    , relocateTo
    , removeIfBlankLeaf
    , restructureCurrentNode
    , restructureWithContext
    , setTitleUnlessBlank
    , unIndent
    , zoomIn
    , zoomOut
    )

import Forest.Zipper
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


new =
    FIZ.new |> Random.map OutlineDoc


zoomIn : OutlineDoc -> Maybe OutlineDoc
zoomIn =
    mapMaybe (Forest.Zipper.forest >> Forest.Zipper.fromForest)


zoomOut : OutlineDoc -> OutlineDoc -> OutlineDoc
zoomOut (OutlineDoc zoomDoc) =
    -- TODO: Need to ensure invariants, i.e. unique ItemID
    -- write quick and dirty func.
    map (Forest.Zipper.replaceChildForest zoomDoc)


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


gotoId : ItemId -> OutlineDoc -> Maybe OutlineDoc
gotoId itemId =
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


expandOrGoForward : OutlineDoc -> Maybe OutlineDoc
expandOrGoForward =
    mapMaybe (Maybe.Extra.oneOf [ FIZ.expand, FIZ.goForward ])


collapse : OutlineDoc -> Maybe OutlineDoc
collapse =
    mapMaybe FIZ.collapse


collapseOrNavParent : OutlineDoc -> Maybe OutlineDoc
collapseOrNavParent =
    mapMaybe (Maybe.Extra.oneOf [ FIZ.collapse, FIZ.goUp ])


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


unIndent : OutlineDoc -> Maybe OutlineDoc
unIndent =
    -- moveAfterParent
    relocateBy FIZ.After FIZ.goUp


indent : OutlineDoc -> Maybe OutlineDoc
indent =
    -- appendInPreviousSibling
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


relocateTo : CandidateLocation -> OutlineDoc -> Maybe OutlineDoc
relocateTo (CandidateLocation loc itemId) =
    mapMaybe (FIZ.relocate loc itemId)


wrapRender render a b c =
    render ( { id = a.id, title = a.title, collapsed = c }, List.map .id b )


restructureWithContext render =
    unwrap >> FIZ.restructureWithContext (wrapRender render)


restructureCurrentNode render =
    unwrap >> FIZ.restructureCursorWithContext (wrapRender render)


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
