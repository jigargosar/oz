module OutlineDoc exposing
    ( CandidateLocation
    , LineInfo
    , OutlineDoc
    , ZoomAncestor
    , addNew
    , after
    , ancestorIds
    , appendIn
    , before
    , candidateLocationDecoder
    , candidateLocationEncoder
    , collapse
    , currentTitle
    , decoder
    , encoder
    , expand
    , goBackward
    , goForward
    , gotoId
    , gotoParent
    , indent
    , isCurrent
    , moveDownwards
    , moveUpwards
    , new
    , prependIn
    , relocateTo
    , removeIfBlankLeaf
    , setTitleUnlessBlank
    , unIndent
    , view
    , viewCurrent
    , zoomAncestors
    , zoomIn
    , zoomOut
    )

import CollapseState exposing (CollapseState)
import Forest.Zipper as Z exposing (ForestZipper, Location(..))
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import OutlineDoc.FIZ as FIZ exposing (FIZ, Item)
import OutlineDoc.Internal exposing (Unwrapped(..), initDoc, initZoomed, unpack)
import Random exposing (Generator)
import Utils exposing (..)



-- CANDIDATE LOCATION


type CandidateLocation
    = CandidateLocation Location ItemId


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



-- DOC MODEL


type alias OutlineDoc =
    OutlineDoc.Internal.OutlineDoc


new : Generator OutlineDoc
new =
    zNew |> Random.map initDoc


zNew : Generator FIZ
zNew =
    FIZ.newLeaf |> Random.map Z.fromTree


zoomIn : OutlineDoc -> Maybe OutlineDoc
zoomIn doc =
    case unpack doc of
        Doc z ->
            Z.childrenAsZipper z |> Maybe.map (initZoomed z)

        Zoomed pz z ->
            Z.merge z pz
                |> (\newPZ -> Z.childrenAsZipper newPZ |> Maybe.map (initZoomed newPZ))


zoomOut : OutlineDoc -> Maybe OutlineDoc
zoomOut doc =
    case unpack doc of
        Doc _ ->
            Nothing

        Zoomed pz z ->
            case Z.transferOneLevelTo z pz of
                ( newZ, Just newPZ ) ->
                    Just (initZoomed newPZ (zExpandAncestors newZ))

                ( newZ, Nothing ) ->
                    Just (initDoc (zExpandAncestors newZ))


encoder : OutlineDoc -> Value
encoder doc =
    case unpack doc of
        Doc z ->
            FIZ.encoder z

        Zoomed pz z ->
            JE.object [ ( "tag", JE.string "Zoomed" ), ( "pz", FIZ.encoder pz ), ( "z", FIZ.encoder z ) ]


decoder : Decoder OutlineDoc
decoder =
    JD.oneOf
        [ FIZ.decoder |> JD.map initDoc
        , JD.succeed initZoomed |> required "pz" FIZ.decoder |> required "z" FIZ.decoder
        ]


map : (FIZ -> FIZ) -> OutlineDoc -> OutlineDoc
map func doc =
    case unpack doc of
        Doc z ->
            initDoc (func z)

        Zoomed pz z ->
            func z |> initZoomed pz


mapMaybe : (FIZ -> Maybe FIZ) -> OutlineDoc -> Maybe OutlineDoc
mapMaybe func doc =
    case unpack doc of
        Doc z ->
            func z |> Maybe.map initDoc

        Zoomed pz z ->
            func z |> Maybe.map (initZoomed pz)


unwrap : OutlineDoc -> FIZ
unwrap doc =
    case unpack doc of
        Doc z ->
            z

        Zoomed _ z ->
            z



-- Getters


currentTitle : OutlineDoc -> String
currentTitle =
    unwrap >> zTitle


zTitle : FIZ -> String
zTitle =
    Z.data >> .title


ancestorIds : OutlineDoc -> List ItemId
ancestorIds =
    unwrap >> Z.ancestors >> List.map .id


isCurrent : ItemId -> OutlineDoc -> Bool
isCurrent itemId =
    unwrap >> propEq zId itemId


zId : FIZ -> ItemId
zId =
    Z.data >> .id



-- NEW INSERTIONS


addNew : OutlineDoc -> Generator OutlineDoc
addNew doc =
    case unpack doc of
        Doc z ->
            z |> zAddNew >> Random.map initDoc

        Zoomed pz z ->
            z |> zAddNew >> Random.map (initZoomed pz)


zAddNew : FIZ -> Generator FIZ
zAddNew z =
    let
        insertNewHelper node =
            if zHasVisibleChildren z then
                Z.prependChildGo node z

            else
                Z.insertRightGo node z
    in
    FIZ.newLeaf
        |> Random.map insertNewHelper



-- Update Node


setTitleUnlessBlank : String -> OutlineDoc -> OutlineDoc
setTitleUnlessBlank title =
    map (setTitle title |> ignoreNothing)


removeIfBlankLeaf : OutlineDoc -> OutlineDoc
removeIfBlankLeaf =
    map (zDeleteEmpty |> ignoreNothing)


expand : OutlineDoc -> Maybe OutlineDoc
expand =
    mapMaybe zExpand


collapse : OutlineDoc -> Maybe OutlineDoc
collapse =
    mapMaybe zCollapse


setTitle : String -> FIZ -> Maybe FIZ
setTitle rawTitle fiz =
    let
        setTitleUnsafe title_ model =
            { model | title = title_ }
    in
    case nonBlank rawTitle of
        Just title ->
            Just (Z.mapData (setTitleUnsafe title) fiz)

        Nothing ->
            Nothing


zExpand : FIZ -> Maybe FIZ
zExpand z =
    let
        canExpand =
            not (Z.isLeaf z) && (Z.data z).collapsed
    in
    if canExpand then
        Just (Z.mapData (setCollapsedUnsafe False) z)

    else
        Nothing


zCollapse : FIZ -> Maybe FIZ
zCollapse fiz =
    let
        canCollapse =
            not (Z.isLeaf fiz || (Z.data fiz).collapsed)
    in
    if canCollapse then
        Just (Z.mapData (setCollapsedUnsafe True) fiz)

    else
        Nothing


zExpandAncestors : FIZ -> FIZ
zExpandAncestors =
    Z.mapAncestors (setCollapsedUnsafe False)


setCollapsedUnsafe collapsed model =
    { model | collapsed = collapsed }


zDeleteEmpty : FIZ -> Maybe FIZ
zDeleteEmpty z =
    if nonBlank (zTitle z) == Nothing && Z.isLeaf z then
        Z.remove z

    else
        Nothing



-- MOVE CURSOR


gotoId : ItemId -> OutlineDoc -> Maybe OutlineDoc
gotoId itemId =
    mapMaybe (zGotoIdAndExpandAncestors itemId)


gotoParent : OutlineDoc -> Maybe OutlineDoc
gotoParent =
    mapMaybe Z.up


goForward : OutlineDoc -> Maybe OutlineDoc
goForward =
    mapMaybe zGoForwardToNextVisible


goBackward : OutlineDoc -> Maybe OutlineDoc
goBackward =
    mapMaybe zGoBackwardToPreviousVisible


zGotoIdAndExpandAncestors : ItemId -> FIZ -> Maybe FIZ
zGotoIdAndExpandAncestors itemId =
    zFindId itemId >> Maybe.map zExpandAncestors


zGoBackwardToPreviousVisible : FIZ -> Maybe FIZ
zGoBackwardToPreviousVisible =
    let
        zLastDescendant =
            applyWhileJust (zGotoFirstVisibleChild >> Maybe.map (applyWhileJust zGotoNextVisibleSibling))
    in
    firstOf [ zGotoPreviousVisibleSibling >> Maybe.map zLastDescendant, Z.up ]


zGotoPreviousVisibleSibling : FIZ -> Maybe FIZ
zGotoPreviousVisibleSibling =
    Z.left


zGotoNextVisibleSibling : FIZ -> Maybe FIZ
zGotoNextVisibleSibling =
    Z.right


zGotoFirstVisibleChild : FIZ -> Maybe FIZ
zGotoFirstVisibleChild fiz =
    if zHasVisibleChildren fiz then
        Z.down fiz

    else
        Nothing


zHasVisibleChildren : FIZ -> Bool
zHasVisibleChildren fiz =
    not (Z.isLeaf fiz || (Z.data fiz |> .collapsed))


zGoForwardToNextVisible : FIZ -> Maybe FIZ
zGoForwardToNextVisible =
    firstOf [ zGotoFirstVisibleChild, zGotoNextVisibleSibling, zGotoNextVisibleSiblingOfAncestor ]


zGotoNextVisibleSiblingOfAncestor : FIZ -> Maybe FIZ
zGotoNextVisibleSiblingOfAncestor z =
    case Z.up z of
        Just parentFIZ ->
            case zGotoNextVisibleSibling parentFIZ of
                Just ns ->
                    Just ns

                Nothing ->
                    zGotoNextVisibleSiblingOfAncestor parentFIZ

        Nothing ->
            Nothing



-- MOVE NODE


relocateTo : CandidateLocation -> OutlineDoc -> Maybe OutlineDoc
relocateTo (CandidateLocation loc itemId) =
    mapMaybe (zRelocate loc itemId)


unIndent : OutlineDoc -> Maybe OutlineDoc
unIndent =
    -- moveAfterParent
    mapMaybe (zRelocateBy After Z.up)


indent : OutlineDoc -> Maybe OutlineDoc
indent =
    -- appendInPreviousSibling
    mapMaybe (zRelocateBy AppendChild zGotoPreviousVisibleSibling)


moveUpwards : OutlineDoc -> Maybe OutlineDoc
moveUpwards =
    -- moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent
    mapMaybe
        (firstOf
            [ zRelocateBy Before zGotoPreviousVisibleSibling
            , zRelocateBy AppendChild (Z.up >> Maybe.andThen zGotoPreviousVisibleSibling)
            ]
        )


moveDownwards : OutlineDoc -> Maybe OutlineDoc
moveDownwards =
    -- moveAfterNextSiblingOrPrependInNextSiblingOfParent
    mapMaybe
        (firstOf
            [ zRelocateBy After zGotoNextVisibleSibling
            , zRelocateBy PrependChild (Z.up >> Maybe.andThen zGotoNextVisibleSibling)
            ]
        )


zRelocateBy :
    Location
    -> (FIZ -> Maybe FIZ)
    -> FIZ
    -> Maybe FIZ
zRelocateBy loc findTargetFunc doc =
    case findTargetFunc doc |> Maybe.map zId of
        Just targetId ->
            zRelocate loc targetId doc

        Nothing ->
            Nothing


zRelocate : Location -> ItemId -> FIZ -> Maybe FIZ
zRelocate relativeLocation targetId zipper =
    let
        removedNode =
            Z.tree zipper
    in
    Z.remove zipper
        |> Maybe.andThen (zFindId targetId)
        |> Maybe.map
            (Z.insertAndGoto relativeLocation removedNode >> zExpandAncestors)



-- VIEW


type alias LineInfo =
    { id : ItemId
    , title : String
    , collapseState : CollapseState
    , isAtCursorOrDescendentOfCursor : Bool
    , isAtCursor : Bool
    }


view : (LineInfo -> List a -> a) -> OutlineDoc -> List a
view render =
    unwrap >> zView render


viewCurrent : (LineInfo -> List a -> a) -> OutlineDoc -> List a
viewCurrent render =
    unwrap >> Z.treeAsZipper >> zView render


type alias ZoomAncestor =
    { id : ItemId, title : String }


zoomAncestors : OutlineDoc -> List ZoomAncestor
zoomAncestors doc =
    let
        itemToZoomAncestor : Item -> ZoomAncestor
        itemToZoomAncestor { id, title } =
            { id = id, title = title }
    in
    case unpack doc of
        Doc _ ->
            []

        Zoomed pz _ ->
            Z.data pz
                :: Z.ancestors pz
                |> List.map itemToZoomAncestor


zView : (LineInfo -> List a -> a) -> FIZ -> List a
zView render initialZ =
    let
        cursorId =
            zId initialZ
    in
    initialZ
        |> Z.restructure
            (\z renderedChildren ->
                let
                    nodeInfo =
                        zToNodeInfo cursorId z
                in
                render nodeInfo
                    (if nodeInfo.collapseState == CollapseState.Expanded then
                        renderedChildren

                     else
                        []
                    )
            )


zToNodeInfo : ItemId -> FIZ -> LineInfo
zToNodeInfo cursorId z =
    let
        isCursor =
            zId z == cursorId
    in
    { id = zId z
    , title = zTitle z
    , collapseState =
        case ( Z.isLeaf z, (Z.data z).collapsed ) of
            ( True, _ ) ->
                CollapseState.NoChildren

            ( _, True ) ->
                CollapseState.Collapsed

            ( _, False ) ->
                CollapseState.Expanded
    , isAtCursorOrDescendentOfCursor = isCursor || List.any (idEq cursorId) (Z.ancestors z)
    , isAtCursor = isCursor
    }



-- Zipper Helpers


zFindId : ItemId -> FIZ -> Maybe FIZ
zFindId itemId =
    Z.findFirst (idEq itemId)
