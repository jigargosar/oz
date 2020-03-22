module OutlineDoc.LineZipper exposing (LineZipper, addNew, cursorChanged, decoder, encoder, getId, getTitle, new, remove, setTitle)

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



-- MODEL


type LineZipper
    = LineZipper (ForestZipper Item)



-- Check for invariants before wrapping


checkInvariants =
    True


unwrap : LineZipper -> ForestZipper Item
unwrap (LineZipper z) =
    ensureInvariants z


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
    Z.ancestors >> List.map .collapsed >> List.any identity


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
    newBlankItem |> Random.map Z.fromLeaf |> Random.map wrap


addNew : LineZipper -> Generator LineZipper
addNew =
    unwrap >> addNew_ >> Random.map wrap


addNew_ : ForestZipper Item -> Generator (ForestZipper Item)
addNew_ z =
    let
        addNewHelp node =
            if hasVisibleChildren_ z then
                Z.prependChildGo node z

            else
                Z.insertRightGo node z
    in
    newBlankItem
        |> Random.map (T.singleton >> addNewHelp)


hasVisibleChildren_ z =
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
cursorChanged doc1 doc2 =
    cursorChanged_ (unwrap doc1) (unwrap doc2)


cursorChanged_ z1 z2 =
    neqBy id_ z1 z2 || neqBy ancestorIds_ z1 z2


ancestorIds_ =
    Z.ancestors >> List.map .id



-- ITEM SETTERS


setTitle : String -> LineZipper -> LineZipper
setTitle newTitle =
    unwrap
        >> Z.mapData (\model -> { model | title = newTitle })
        >> wrap



-- Remove


remove : LineZipper -> Maybe LineZipper
remove =
    unwrap >> Z.remove >> Maybe.map wrap
