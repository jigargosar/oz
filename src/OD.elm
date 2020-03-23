module OD exposing (Item, OD, new)

import ItemId exposing (ItemId)
import Random exposing (Generator)


type OD
    = OD (List Crumb) (List Crumb) LTR


type Crumb
    = Crumb (List T) Item (List T)


type LTR
    = LTR (List T) T (List T)


type T
    = T Item (List T)


type Item
    = Item ItemId


new : Generator OD
new =
    ItemId.generator
        |> Random.map newFromId


newFromId : ItemId -> OD
newFromId id =
    OD [] [] (LTR [] (T (Item id) []) [])
