module OutlineDoc.Internal exposing (OutlineDoc, Unwrapped(..), initDoc, initZoomed, open)

import Dict
import FIZ exposing (FIZ)
import Forest.Zipper as Z
import ItemId
import Tree as T


type OutlineDoc
    = Doc_ FIZ
    | Zoomed_ FIZ FIZ


type Unwrapped
    = Doc FIZ
    | Zoomed FIZ FIZ


open : OutlineDoc -> Unwrapped
open doc =
    case doc of
        Doc_ z ->
            Doc z

        Zoomed_ pz z ->
            Zoomed pz z


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
                        |> always (ensureAncestorsExpanded z)
                        |> always ensureUniqueNodes pz
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

        _ =
            Z.rootForest fiz
                |> List.foldl (\t d -> T.foldl safeInsertItem d t) Dict.empty
    in
    fiz
