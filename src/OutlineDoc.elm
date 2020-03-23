module OutlineDoc exposing
    ( CandidateLocation
    , LineInfo
    , OutlineDoc
    , addNew
    , after
    , appendIn
    , before
    , candidateLocationDecoder
    , candidateLocationEncoder
    , collapse
    , collapseAll
    , currentIdEq
    , currentTitle
    , cursorChanged
    , decoder
    , encoder
    , expand
    , expandAll
    , goBackward
    , goForward
    , gotoId
    , gotoParent
    , gotoPreviousSibling
    , indent
    , moveDownwards
    , moveUpwards
    , new
    , prependIn
    , relocateTo
    , removeIfBlankLeaf
    , setTitle
    , setTitleUnlessBlank
    , unIndent
    , view
    , viewCurrent
    , zoomIn
    , zoomOut
    , zoomOutToAncestorId
    , zoomOutToTop
    )

import CollapseState exposing (CollapseState)
import Forest.Tree as Tree
import Forest.Zipper as Z exposing (ForestZipper, Location(..))
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import OutlineDoc.FIZ as FIZ exposing (FIZ, Item)
import OutlineDoc.Internal
    exposing
        ( Unwrapped(..)
        , map
        , mapMaybe
        , unwrap
        , wrap
        )
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


encoder : OutlineDoc -> Value
encoder doc =
    case unwrap doc of
        Doc z ->
            FIZ.encoder z

        Zoomed pz z ->
            JE.object
                [ ( "tag", JE.string "Zoomed" )
                , ( "pz", FIZ.encoder pz )
                , ( "z", FIZ.encoder z )
                ]


decoder : Decoder OutlineDoc
decoder =
    JD.oneOf
        [ FIZ.decoder |> JD.map Doc
        , JD.succeed Zoomed
            |> required "pz" FIZ.decoder
            |> required "z" FIZ.decoder
        ]
        |> JD.map wrap


getCZ : OutlineDoc -> FIZ
getCZ doc =
    case unwrap doc of
        Doc z ->
            z

        Zoomed _ z ->
            z


mapCZ : (FIZ -> FIZ) -> OutlineDoc -> OutlineDoc
mapCZ =
    mapCZ_ >> map


mapCZMaybe : (FIZ -> Maybe FIZ) -> OutlineDoc -> Maybe OutlineDoc
mapCZMaybe func =
    mapMaybe (mapCZMaybe_ func)


mapCZ_ : (FIZ -> FIZ) -> Unwrapped -> Unwrapped
mapCZ_ func unwrapped =
    case unwrapped of
        Doc z ->
            Doc (func z)

        Zoomed pz z ->
            Zoomed pz (func z)


mapCZMaybe_ : (FIZ -> Maybe FIZ) -> Unwrapped -> Maybe Unwrapped
mapCZMaybe_ func unwrapped =
    case unwrapped of
        Doc z ->
            func z |> Maybe.map Doc

        Zoomed pz z ->
            func z |> Maybe.map (Zoomed pz)



-- Getters


currentTitle : OutlineDoc -> String
currentTitle =
    getCZ >> zTitle


zTitle : FIZ -> String
zTitle =
    Z.data >> .title


cursorChanged : OutlineDoc -> OutlineDoc -> Bool
cursorChanged doc1 doc2 =
    zCursorChanged (getCZ doc1) (getCZ doc2)


zCursorChanged : FIZ -> FIZ -> Bool
zCursorChanged z1 z2 =
    neqBy zId z1 z2 || neqBy zAncestorIds z1 z2


zAncestorIds : FIZ -> List ItemId
zAncestorIds =
    Z.ancestors >> List.map .id


currentIdEq : ItemId -> OutlineDoc -> Bool
currentIdEq itemId =
    getCZ >> propEq zId itemId


zId : FIZ -> ItemId
zId =
    Z.data >> .id



-- ZOOM


zoomIn : OutlineDoc -> Maybe OutlineDoc
zoomIn =
    always Nothing


zoomOut : OutlineDoc -> Maybe OutlineDoc
zoomOut =
    always Nothing


zoomOutToTop : OutlineDoc -> Maybe OutlineDoc
zoomOutToTop =
    always Nothing


zoomOutToAncestorId : ItemId -> OutlineDoc -> Maybe OutlineDoc
zoomOutToAncestorId _ =
    always Nothing


zIsVisible : FIZ -> Bool
zIsVisible =
    Z.ancestors >> List.any .collapsed >> not


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



-- NEW INSERTIONS


addNew : OutlineDoc -> Generator OutlineDoc
addNew =
    let
        addNewHelp func z =
            Random.map func (zAddNew z)
    in
    unwrap
        >> (\doc ->
                case doc of
                    Doc z ->
                        addNewHelp Doc z

                    Zoomed pz z ->
                        addNewHelp (Zoomed pz) z
           )
        >> Random.map wrap


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
    mapCZ (setNonBlankTitle title |> ignoreNothing)


setTitle : String -> OutlineDoc -> OutlineDoc
setTitle newTitle =
    mapCZ (Z.mapData (\model -> { model | title = newTitle }))


removeIfBlankLeaf : OutlineDoc -> OutlineDoc
removeIfBlankLeaf =
    mapCZ (zDeleteBlankLeaf |> ignoreNothing)



--noinspection ElmUnusedSymbol


removeLeaf : OutlineDoc -> Maybe OutlineDoc
removeLeaf =
    mapCZMaybe zDeleteLeaf


expandAll : OutlineDoc -> Maybe OutlineDoc
expandAll =
    mapCZMaybe
        (zMap (\model -> { model | collapsed = False })
            >> Maybe.map zGotoFirstVisibleAncestor
        )


collapseAll : OutlineDoc -> Maybe OutlineDoc
collapseAll =
    mapCZMaybe
        (zMap (\model -> { model | collapsed = True })
            >> Maybe.map zGotoFirstVisibleAncestor
        )


zMap : (Item -> Item) -> FIZ -> Maybe FIZ
zMap func z =
    Z.rootForest z
        |> List.map (Tree.map func)
        |> Z.fromForest
        |> Maybe.andThen (zFindId (zId z))


expand : OutlineDoc -> Maybe OutlineDoc
expand =
    mapCZMaybe zExpand


collapse : OutlineDoc -> Maybe OutlineDoc
collapse =
    mapCZMaybe zCollapse


setNonBlankTitle : String -> FIZ -> Maybe FIZ
setNonBlankTitle rawTitle fiz =
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


zDeleteBlankLeaf : FIZ -> Maybe FIZ
zDeleteBlankLeaf z =
    if nonBlank (zTitle z) == Nothing && Z.isLeaf z then
        Z.remove z

    else
        Nothing


zDeleteLeaf : FIZ -> Maybe FIZ
zDeleteLeaf z =
    if Z.isLeaf z then
        Z.remove z

    else
        Nothing



-- MOVE CURSOR


gotoId : ItemId -> OutlineDoc -> Maybe OutlineDoc
gotoId itemId =
    mapCZMaybe (zGotoIdAndExpandAncestors itemId)


gotoParent : OutlineDoc -> Maybe OutlineDoc
gotoParent =
    mapCZMaybe Z.up


gotoPreviousSibling : OutlineDoc -> Maybe OutlineDoc
gotoPreviousSibling =
    mapCZMaybe zGotoPreviousVisibleSibling


goForward : OutlineDoc -> Maybe OutlineDoc
goForward =
    mapCZMaybe zGoForwardToNextVisible


goBackward : OutlineDoc -> Maybe OutlineDoc
goBackward =
    mapCZMaybe zGoBackwardToPreviousVisible


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
    mapCZMaybe (zRelocate loc itemId)


unIndent : OutlineDoc -> Maybe OutlineDoc
unIndent =
    -- moveAfterParent
    mapCZMaybe (zRelocateBy After Z.up)


indent : OutlineDoc -> Maybe OutlineDoc
indent =
    -- appendInPreviousSibling
    mapCZMaybe (zRelocateBy AppendChild zGotoPreviousVisibleSibling)


moveUpwards : OutlineDoc -> Maybe OutlineDoc
moveUpwards =
    -- moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent
    mapCZMaybe
        (firstOf
            [ zRelocateBy Before zGotoPreviousVisibleSibling
            , zRelocateBy AppendChild (Z.up >> Maybe.andThen zGotoPreviousVisibleSibling)
            ]
        )


moveDownwards : OutlineDoc -> Maybe OutlineDoc
moveDownwards =
    -- moveAfterNextSiblingOrPrependInNextSiblingOfParent
    mapCZMaybe
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
    getCZ >> zView render


viewCurrent : (LineInfo -> List a -> a) -> OutlineDoc -> List a
viewCurrent render =
    getCZ >> Z.treeAsZipper >> zView render


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
