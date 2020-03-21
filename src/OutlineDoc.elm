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
    , currentIdEq
    , currentTitle
    , decoder
    , encoder
    , expand
    , goBackward
    , goForward
    , gotoId
    , gotoParent
    , indent
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
    , zoomTitle
    )

import CollapseState exposing (CollapseState)
import Forest.Zipper as Z exposing (ForestZipper, Location(..))
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import OutlineDoc.FIZ as FIZ exposing (FIZ, Item)
import OutlineDoc.Internal exposing (Unwrapped(..), initDoc, initZoomed, map, mapMaybe, unwrap, wrap)
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
    zNew |> Random.map (Doc >> wrap)


zNew : Generator FIZ
zNew =
    FIZ.newLeaf |> Random.map Z.fromTree


mapUnwrappedChildZipper : (FIZ -> FIZ) -> Unwrapped -> Unwrapped
mapUnwrappedChildZipper func unwrapped =
    case unwrapped of
        Doc z ->
            Doc (func z)

        Zoomed pz z ->
            Zoomed pz (func z)


encoder : OutlineDoc -> Value
encoder doc =
    case unwrap doc of
        Doc z ->
            FIZ.encoder z

        Zoomed pz z ->
            JE.object [ ( "tag", JE.string "Zoomed" ), ( "pz", FIZ.encoder pz ), ( "z", FIZ.encoder z ) ]


decoder : Decoder OutlineDoc
decoder =
    JD.oneOf
        [ FIZ.decoder |> JD.map Doc
        , JD.succeed Zoomed |> required "pz" FIZ.decoder |> required "z" FIZ.decoder
        ]
        |> JD.map wrap


mapChildZipper : (FIZ -> FIZ) -> OutlineDoc -> OutlineDoc
mapChildZipper func =
    map
        (\unwrapped ->
            case unwrapped of
                Doc z ->
                    Doc (func z)

                Zoomed pz z ->
                    Zoomed pz (func z)
        )


mapMaybeChildZipper : (FIZ -> Maybe FIZ) -> OutlineDoc -> Maybe OutlineDoc
mapMaybeChildZipper func =
    mapMaybe
        (\unwrapped ->
            case unwrapped of
                Doc z ->
                    func z |> Maybe.map Doc

                Zoomed pz z ->
                    func z |> Maybe.map (Zoomed pz)
        )


getChildZipper : OutlineDoc -> FIZ
getChildZipper doc =
    case unwrap doc of
        Doc z ->
            z

        Zoomed _ z ->
            z



-- Getters


currentTitle : OutlineDoc -> String
currentTitle =
    getChildZipper >> zTitle


zTitle : FIZ -> String
zTitle =
    Z.data >> .title


ancestorIds : OutlineDoc -> List ItemId
ancestorIds =
    getChildZipper >> Z.ancestors >> List.map .id


currentIdEq : ItemId -> OutlineDoc -> Bool
currentIdEq itemId =
    getChildZipper >> propEq zId itemId


zId : FIZ -> ItemId
zId =
    Z.data >> .id


type alias ZoomInfo =
    { ancestors : List ZoomAncestor
    , current : ZoomAncestor
    }


type alias ZoomAncestor =
    { id : ItemId, title : String }


zoomInfo : OutlineDoc -> Maybe ZoomInfo
zoomInfo =
    let
        helper pz =
            Nothing
    in
    getParentZipper >> helper


zoomAncestors : OutlineDoc -> List ZoomAncestor
zoomAncestors doc =
    let
        itemToZoomAncestor : Item -> ZoomAncestor
        itemToZoomAncestor { id, title } =
            { id = id, title = title }
    in
    case unwrap doc of
        Doc _ ->
            []

        Zoomed pz _ ->
            Z.data pz
                :: Z.ancestors pz
                |> List.map itemToZoomAncestor


zoomTitle : OutlineDoc -> Maybe String
zoomTitle =
    getParentZipper >> Maybe.map zTitle


getParentZipper : OutlineDoc -> Maybe FIZ
getParentZipper doc =
    case unwrap doc of
        Zoomed pz _ ->
            Just pz

        _ ->
            Nothing



-- ZOOM


zoomIn : OutlineDoc -> Maybe OutlineDoc
zoomIn =
    mapMaybe
        (\doc ->
            case doc of
                Doc z ->
                    Z.childrenAsZipper z |> Maybe.map (Zoomed z)

                Zoomed pz z ->
                    Z.merge z pz
                        |> (\newPZ -> Z.childrenAsZipper newPZ |> Maybe.map (Zoomed newPZ))
        )


zoomOut : OutlineDoc -> Maybe OutlineDoc
zoomOut =
    mapMaybe
        (zoomOutUnwrapped
            >> Maybe.map (mapUnwrappedChildZipper zGotoFirstVisibleAncestor)
        )


zoomOutUnwrapped : Unwrapped -> Maybe Unwrapped
zoomOutUnwrapped unwrapped =
    case unwrapped of
        Doc _ ->
            Nothing

        Zoomed pz z ->
            case Z.transferOneLevelTo z pz of
                ( newZ, Just newPZ ) ->
                    Just (Zoomed newPZ newZ)

                ( newZ, Nothing ) ->
                    Just (Doc newZ)


zGotoFirstVisibleAncestor : FIZ -> FIZ
zGotoFirstVisibleAncestor z =
    if zIsVisible z then
        z

    else
        case Z.up z of
            Just pz ->
                zGotoFirstVisibleAncestor pz

            Nothing ->
                z


zIsVisible : FIZ -> Bool
zIsVisible =
    Z.ancestors >> List.any .collapsed >> not



-- NEW INSERTIONS


addNew : OutlineDoc -> Generator OutlineDoc
addNew doc =
    (case unwrap doc of
        Doc z ->
            z |> zAddNew >> Random.map Doc

        Zoomed pz z ->
            z |> zAddNew >> Random.map (Zoomed pz)
    )
        |> Random.map wrap


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
    mapChildZipper (setTitle title |> ignoreNothing)


removeIfBlankLeaf : OutlineDoc -> OutlineDoc
removeIfBlankLeaf =
    mapChildZipper (zDeleteEmpty |> ignoreNothing)


expand : OutlineDoc -> Maybe OutlineDoc
expand =
    mapMaybeChildZipper zExpand


collapse : OutlineDoc -> Maybe OutlineDoc
collapse =
    mapMaybeChildZipper zCollapse


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
    mapMaybeChildZipper (zGotoIdAndExpandAncestors itemId)


gotoParent : OutlineDoc -> Maybe OutlineDoc
gotoParent =
    mapMaybeChildZipper Z.up


goForward : OutlineDoc -> Maybe OutlineDoc
goForward =
    mapMaybeChildZipper zGoForwardToNextVisible


goBackward : OutlineDoc -> Maybe OutlineDoc
goBackward =
    mapMaybeChildZipper zGoBackwardToPreviousVisible


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
    mapMaybeChildZipper (zRelocate loc itemId)


unIndent : OutlineDoc -> Maybe OutlineDoc
unIndent =
    -- moveAfterParent
    mapMaybeChildZipper (zRelocateBy After Z.up)


indent : OutlineDoc -> Maybe OutlineDoc
indent =
    -- appendInPreviousSibling
    mapMaybeChildZipper (zRelocateBy AppendChild zGotoPreviousVisibleSibling)


moveUpwards : OutlineDoc -> Maybe OutlineDoc
moveUpwards =
    -- moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent
    mapMaybeChildZipper
        (firstOf
            [ zRelocateBy Before zGotoPreviousVisibleSibling
            , zRelocateBy AppendChild (Z.up >> Maybe.andThen zGotoPreviousVisibleSibling)
            ]
        )


moveDownwards : OutlineDoc -> Maybe OutlineDoc
moveDownwards =
    -- moveAfterNextSiblingOrPrependInNextSiblingOfParent
    mapMaybeChildZipper
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
    getChildZipper >> zView render


viewCurrent : (LineInfo -> List a -> a) -> OutlineDoc -> List a
viewCurrent render =
    getChildZipper >> Z.treeAsZipper >> zView render


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
