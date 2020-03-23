module OutlineDoc.Internal exposing
    ( OutlineDoc
    , Unwrapped(..)
    , ZZ
    , initDoc
    , initZoomed
    , map
    , mapMaybe
    , unwrap
    , unwrapZZ
    , wrap
    , wrapZZ
    )

import Dict
import Forest.Zipper as Z
import Forest.ZoomZipper as ZZ exposing (ZoomZipper)
import ItemId
import OutlineDoc.FIZ exposing (FIZ, Item)
import Tree as T
import Utils exposing (..)


type alias ZZ =
    ZoomZipper Item


unwrapZZ : OutlineDoc -> ZZ
unwrapZZ doc =
    checkWrapZZ doc <| unwrapZZ_ doc


wrapZZ : ZZ -> OutlineDoc
wrapZZ zz =
    checkUnwrapZZ zz <| wrapZZ_ zz


checkWrapZZ : OutlineDoc -> ZZ -> ZZ
checkWrapZZ doc zz =
    if wrapZZ_ zz == doc then
        zz

    else
        let
            _ =
                wrapZZ_ zz |> Debug.log "debug"

            _ =
                doc |> Debug.log "debug"
        in
        Debug.todo "checkWrapZZ failed during unwrapZZ"


checkUnwrapZZ zz doc =
    if unwrapZZ_ doc == zz then
        doc

    else
        let
            _ =
                unwrapZZ_ doc |> Debug.log "debug"

            _ =
                zz |> Debug.log "debug"
        in
        Debug.todo "impl"


unwrapZZ_ : OutlineDoc -> ZZ
unwrapZZ_ doc =
    case doc of
        Doc_ z ->
            case
                Z.rootForestTuple z
                    |> uncurry ZZ.fromCons
                    |> ZZ.findFirst (eqById (Z.data z))
            of
                Just zz ->
                    zz

                Nothing ->
                    Debug.todo "impl"

        Zoomed_ pz z ->
            Z.transferAllLevelsFrom pz z |> toZZPreserveFocusAndZoom (Z.data pz) (Z.data z)


wrapZZ_ : ZZ -> OutlineDoc
wrapZZ_ zz =
    case ZZ.zoomData zz of
        Just item ->
            case
                zz
                    |> applyWhileJust ZZ.up
                    |> ZZ.forest
                    |> Z.fromForest
                    |> Maybe.andThen (Z.findFirst (eqById (ZZ.data zz)))
            of
                Just fiz ->
                    initZoomed
                        (case
                            ZZ.rootForest zz
                                |> Z.fromForest
                                |> Maybe.andThen (Z.findFirst (eqById item))
                         of
                            Just z ->
                                z

                            Nothing ->
                                Debug.todo "impl"
                        )
                        fiz

                Nothing ->
                    Debug.todo "impl"

        Nothing ->
            case
                ZZ.rootForest zz
                    |> Z.fromForest
                    |> Maybe.andThen (Z.findFirst (eqById (ZZ.data zz)))
            of
                Just fiz ->
                    initDoc fiz

                Nothing ->
                    Debug.todo "impl"


toZZPreserveFocusAndZoom zoomItem focusItem fiz =
    case
        fiz
            |> Z.rootForestTuple
            |> uncurry ZZ.fromCons
            |> ZZ.findFirst (eqById zoomItem)
            |> Maybe.andThen ZZ.zoomIn
            |> Maybe.andThen (ZZ.findFirst (eqById focusItem))
    of
        Just zz ->
            zz

        Nothing ->
            Debug.todo "impl"


type OutlineDoc
    = Doc_ FIZ
    | Zoomed_ FIZ FIZ


type Unwrapped
    = Doc FIZ
    | Zoomed FIZ FIZ


unwrap : OutlineDoc -> Unwrapped
unwrap doc =
    case doc of
        Doc_ z ->
            Doc z

        Zoomed_ pz z ->
            Zoomed pz z


wrap : Unwrapped -> OutlineDoc
wrap unwrapped =
    case unwrapped of
        Doc z ->
            initDoc z

        Zoomed pz z ->
            initZoomed pz z


map : (Unwrapped -> Unwrapped) -> OutlineDoc -> OutlineDoc
map func =
    unwrap >> func >> wrap


mapMaybe : (Unwrapped -> Maybe Unwrapped) -> OutlineDoc -> Maybe OutlineDoc
mapMaybe func =
    unwrap >> func >> Maybe.map wrap


initZoomed : FIZ -> FIZ -> OutlineDoc
initZoomed pz z =
    Zoomed_ pz z
        |> ensureDocInvariants


initDoc : FIZ -> OutlineDoc
initDoc z =
    Doc_ z
        |> ensureDocInvariants


ensureDocInvariants : OutlineDoc -> OutlineDoc
ensureDocInvariants doc =
    let
        _ =
            case doc of
                Doc_ z ->
                    ensureUniqueNodes z
                        |> always (ensureAncestorsExpanded z)

                Zoomed_ pz z ->
                    ensureUniqueNodes z
                        |> always (ensureUniqueNodesHelp pz)
                        |> always (ensureAncestorsExpanded z)
                        |> always (Z.transferAllLevelsFrom pz z |> ensureUniqueNodes)
    in
    doc


ensureAncestorsExpanded : FIZ -> FIZ
ensureAncestorsExpanded fiz =
    let
        _ =
            if Z.ancestors fiz |> List.map .collapsed |> List.any identity then
                Debug.todo "ancestors should be expanded, invariant failed"

            else
                1
    in
    fiz


ensureUniqueNodes : FIZ -> FIZ
ensureUniqueNodes fiz =
    let
        _ =
            ensureUniqueNodesHelp fiz Dict.empty
    in
    fiz


ensureUniqueNodesHelp fiz dict =
    Z.rootForest fiz
        |> List.foldl (\t d -> T.foldl safeInsertItem d t) dict


safeInsertItem item d =
    let
        strId =
            ItemId.toString item.id

        _ =
            case Dict.get strId d of
                Just _ ->
                    Debug.todo (Debug.toString ( "duplicate item found", item, d ))

                Nothing ->
                    1
    in
    Dict.insert strId item d
