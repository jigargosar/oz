module OutlineDoc.LineZipper exposing (LineZipper, addNew, collapseAll, cursorChanged, decoder, encoder, expandAll, getId, getTitle, new, remove, setTitle)

import Forest.Tree as T exposing (Forest, Tree)
import Forest.Zipper as Z exposing (ForestZipper, Location)
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List.Extra
import Random exposing (Generator)
import Utils exposing (..)



-- ITEM


type alias Item =
    { id : ItemId
    , title : String
    , collapsed : Bool
    }


strId =
    .id >> ItemId.toString


itemGenerator : String -> Generator Item
itemGenerator title =
    ItemId.generator
        |> Random.map (\id -> { id = id, title = title, collapsed = False })


newBlankItem : Generator Item
newBlankItem =
    itemGenerator ""


itemEncoder : Item -> Value
itemEncoder item =
    JE.object
        [ ( "id", ItemId.itemIdEncoder item.id )
        , ( "title", JE.string item.title )
        , ( "collapsed", JE.bool item.collapsed )
        ]


itemDecoder : Decoder Item
itemDecoder =
    JD.succeed Item
        |> required "id" ItemId.itemIdDecoder
        |> required "title" JD.string
        |> JD.map2 (|>) (JD.oneOf [ JD.field "collapsed" JD.bool, JD.succeed False ])


setItemTitle : String -> Item -> Item
setItemTitle title model =
    { model | title = title }


setItemCollapsed : Bool -> Item -> Item
setItemCollapsed collapsed model =
    { model | collapsed = collapsed }



-- MODEL


type LineZipper
    = LineZipper (ForestZipper Item)



-- Check for invariants before wrapping


checkInvariants =
    True


unwrap : LineZipper -> ForestZipper Item
unwrap (LineZipper z) =
    z


wrap : ForestZipper Item -> LineZipper
wrap z =
    LineZipper (ensureInvariants z)


ensureInvariants : ForestZipper Item -> ForestZipper Item
ensureInvariants z =
    validate z
        |> Result.mapError
            (if checkInvariants then
                Debug.todo

             else
                Debug.log "invariant failed"
            )
        |> Result.withDefault z


validate : ForestZipper Item -> Result String (ForestZipper Item)
validate z =
    if hasCollapsedAncestors z then
        Err "hasCollapsedAncestors"

    else if hasDuplicateItemIds z then
        Err "hasDuplicateItemIds"

    else
        Ok z


hasCollapsedAncestors : ForestZipper Item -> Bool
hasCollapsedAncestors =
    Z.ancestors >> List.any .collapsed


hasDuplicateItemIds : ForestZipper { a | id : ItemId } -> Bool
hasDuplicateItemIds =
    Z.foldl (::) [] >> List.Extra.allDifferentBy strId >> not



-- JSON


encoder : LineZipper -> Value
encoder =
    unwrap >> Z.encoder itemEncoder


decoder : Decoder LineZipper
decoder =
    Z.decoder itemDecoder |> JD.map wrap



-- Generators


new : Generator LineZipper
new =
    newBlankItem |> Random.map (Z.fromLeaf >> wrap)


addNew : LineZipper -> Generator LineZipper
addNew (LineZipper z) =
    let
        addFunc =
            if hasVisibleChildren z then
                Z.prependChildGo

            else
                Z.insertRightGo

        insertNew newItem =
            addFunc (T.singleton newItem) z
    in
    newBlankItem
        |> Random.map (insertNew >> wrap)


hasVisibleChildren z =
    not (Z.isLeaf z || collapsed_ z)



-- QUERIES


collapsed_ =
    Z.data >> .collapsed


getId : LineZipper -> ItemId
getId =
    unwrap >> id_


id_ =
    Z.data >> .id


getTitle : LineZipper -> String
getTitle =
    unwrap >> title_


title_ =
    Z.data >> .title


cursorChanged : LineZipper -> LineZipper -> Bool
cursorChanged (LineZipper z1) (LineZipper z2) =
    neqBy id_ z1 z2 || neqBy ancestorIds z1 z2


ancestorIds =
    Z.ancestors >> List.map .id



-- ITEM SETTERS


setTitle : String -> LineZipper -> LineZipper
setTitle newTitle (LineZipper z) =
    z
        |> Z.mapData (setItemTitle newTitle)
        |> wrap



-- Remove


remove : LineZipper -> Maybe LineZipper
remove (LineZipper z) =
    z |> Z.remove |> Maybe.map wrap



-- UPDATE


expandAll : LineZipper -> Maybe LineZipper
expandAll (LineZipper z) =
    z |> Z.map (setItemCollapsed False) |> wrap |> Just


collapseAll : LineZipper -> Maybe LineZipper
collapseAll (LineZipper z) =
    z |> Z.map (setItemCollapsed True) |> wrap |> Just