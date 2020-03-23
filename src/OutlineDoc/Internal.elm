module OutlineDoc.Internal exposing
    ( OutlineDoc
    , Unwrapped(..)
    , Unwrapped2
    , ZZ
    , initDoc
    , initZoomed
    , map
    , mapMaybe
    , unwrap
    , unwrap2
    , unwrapZZ
    , wrap
    , wrap2
    )

import Dict
import Forest.Zipper as Z
import Forest.ZoomZipper as ZZ exposing (ZoomZipper)
import ItemId
import OutlineDoc.FIZ exposing (FIZ, Item)
import Tree as T
import Utils exposing (..)


type alias Unwrapped2 =
    ( Maybe FIZ, FIZ )


unwrap2 : OutlineDoc -> Unwrapped2
unwrap2 doc =
    case doc of
        Doc_ z ->
            ( Nothing, z )

        Zoomed_ pz z ->
            ( Just pz, z )


wrap2 : Unwrapped2 -> OutlineDoc
wrap2 ( mpz, z ) =
    case mpz of
        Just pz ->
            initZoomed pz z

        Nothing ->
            initDoc z


type alias ZZ =
    ZoomZipper Item


unwrapZZ : OutlineDoc -> ZZ
unwrapZZ doc =
    case doc of
        Doc_ z ->
            Z.rootForestTuple z |> uncurry ZZ.fromCons

        Zoomed_ pz z ->
            Z.mergeChild z pz |> toZZPreserveFocusAndZoom (Z.data pz)


wrapZZ : ZZ -> OutlineDoc
wrapZZ zz =
    Debug.todo "impl"


toZZPreserveFocusAndZoom zoomItem fiz =
    case
        fiz
            |> Z.rootForestTuple
            |> uncurry ZZ.fromCons
            |> ZZ.findFirst (eqById zoomItem)
            |> Maybe.andThen ZZ.zoomIn
            |> Maybe.andThen (ZZ.findFirst (eqById (Z.data fiz)))
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
    let
        zz =
            unwrapZZ doc |> ZZ.unConsRoot

        fiz =
            unwrap2 doc
                |> (\( mpz, z ) ->
                        case mpz of
                            Just pz ->
                                Z.mergeChild z pz

                            Nothing ->
                                z
                   )
                |> Z.rootForestTuple

        _ =
            Debug.log "debug" (fiz == zz)
    in
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
