module OutlineDoc.Internal exposing (OutlineDoc, Unwrapped(..), initDoc, initZoomed, unwrap)

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


unwrap : OutlineDoc -> Unwrapped
unwrap doc =
    case doc of
        Doc_ z ->
            Doc z

        Zoomed_ pz z ->
            Zoomed pz z


initZoomed : FIZ -> FIZ -> OutlineDoc
initZoomed pz z =
    Zoomed_ pz z
        |> ensureInvariants


initDoc : FIZ -> OutlineDoc
initDoc z =
    Doc_ z
        |> ensureInvariants


ensureInvariants : OutlineDoc -> OutlineDoc
ensureInvariants doc =
    let
        _ =
            case doc of
                Doc_ z ->
                    ensureUniqueNodes z

                Zoomed_ pz z ->
                    ensureUniqueNodes z
                        |> always (ensureUniqueNodes pz)
    in
    doc


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
