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


wrapZZ : ZZ -> OutlineDoc
wrapZZ zz =
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


zoomData : OutlineDoc -> Maybe Item
zoomData doc =
    case doc of
        Doc_ _ ->
            Nothing

        Zoomed_ pz _ ->
            Just (Z.data pz)


unwrap : OutlineDoc -> Unwrapped
unwrap doc =
    let
        _ =
            if doc /= wrapZZ (unwrapZZ doc) then
                Debug.log "neq" (wrapZZ (unwrapZZ doc))

            else
                doc

        _ =
            case ( doc, wrapZZ (unwrapZZ doc) ) of
                ( Zoomed_ p1 c1, Zoomed_ p2 c2 ) ->
                    --Debug.log "peq" (p1 == p2)
                    --    |> always
                    Debug.log "ceq" (c1 == c2)
                        |> always ""

                _ ->
                    ""

        zzRF =
            unwrapZZ doc |> ZZ.rootForest

        fizRF =
            unwrap2 doc
                |> (\( mpz, z ) ->
                        case mpz of
                            Just pz ->
                                Z.mergeChild z pz

                            Nothing ->
                                z
                   )
                |> Z.rootForest

        _ =
            if zzRF /= fizRF then
                Debug.log "neq" ( zzRF, fizRF )
                    |> always never

            else
                always never
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
