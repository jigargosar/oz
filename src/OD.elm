module OD exposing (Item, OD, addNew, new)

import ItemId exposing (ItemId)
import Random exposing (Generator)
import Utils exposing (flip)


type OD
    = OD (List Crumb) (List Crumb) LTR


type Crumb
    = Crumb (List T) Item (List T)


type LTR
    = LTR (List T) T (List T)


type T
    = T Item (List T)


type Item
    = Item ItemId Bool


itemFromId id =
    Item id False


itemCollapsed (Item _ c) =
    c


treeFromId id =
    T (itemFromId id) []


treeHasExpandedChildren (T item ts) =
    List.isEmpty ts || itemCollapsed item


new : Generator OD
new =
    ItemId.generator
        |> Random.map newFromId


newFromId : ItemId -> OD
newFromId id =
    OD [] [] (LTR [] (treeFromId id) [])


addNew : OD -> Generator OD
addNew od =
    ItemId.generator
        |> Random.map (flip addNewWithId od)


addNewWithId : ItemId -> OD -> OD
addNewWithId id (OD pcs cs (LTR l t r)) =
    let
        newT =
            treeFromId id
    in
    if treeHasExpandedChildren t then
        -- insertAfter
        OD pcs cs (LTR (t :: l) newT r)

    else
        -- prepend child
        let
            (T item children) =
                t

            newCrumb =
                Crumb l item r

            newLTR =
                LTR [] newT children
        in
        OD pcs (newCrumb :: cs) newLTR
