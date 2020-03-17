module ItemForestZipper exposing
    ( FIZ
    , Item
    , ItemId
    , appendInPreviousSibling
    , currentId
    , currentTitle
    , decoder
    , encoder
    , goBackward
    , goForward
    , hasVisibleChildren
    , itemIdDecoder
    , itemIdEncoder
    , moveAfterNextSiblingOrPrependInNextSiblingOfParent
    , moveAfterParent
    , moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent
    , moveCursorToItemId
    , newChild
    , newSibling
    , removeIfBlankLeaf
    , restructure
    , restructureFocused
    , setTitleUnlessBlank
    )

import Forest.Tree as Tree exposing (Forest, Tree)
import Forest.Zipper as Zipper exposing (ForestZipper)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Maybe.Extra
import Random exposing (Generator)



-- CANDIDATE LOCATION


type Location
    = Before
    | After
    | PrependChild
    | AppendChild



-- ITEM


type alias Item =
    { id : ItemId
    , title : String
    }


itemEncoder : Item -> Value
itemEncoder item =
    JE.object
        [ ( "id", itemIdEncoder item.id )
        , ( "title", JE.string item.title )
        ]


itemDecoder : Decoder Item
itemDecoder =
    JD.succeed Item
        |> required "id" itemIdDecoder
        |> required "title" JD.string


type ItemId
    = ItemId String


itemGenerator : String -> Generator Item
itemGenerator title =
    itemIdGenerator
        |> Random.map (\id -> { id = id, title = title })


itemIdGenerator : Generator ItemId
itemIdGenerator =
    Random.int 10000 Random.maxInt
        |> Random.map (String.fromInt >> (++) "item-id-" >> ItemId)


itemIdEncoder : ItemId -> Value
itemIdEncoder (ItemId string) =
    JE.string string


itemIdDecoder : Decoder ItemId
itemIdDecoder =
    JD.string
        |> JD.andThen
            (\idStr ->
                if String.startsWith "item-id-" idStr then
                    JD.succeed (ItemId idStr)

                else
                    JD.fail ("invalid item id prefix: " ++ idStr)
            )



-- MODEL


type alias FIZ =
    FIZ


encoder : FIZ -> Value
encoder zipper =
    Zipper.encoder itemEncoder zipper


decoder : Decoder FIZ
decoder =
    Zipper.decoder itemDecoder


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required fieldName decoder_ =
    JD.map2 (|>) (JD.field fieldName decoder_)


emptyLeafGenerator : Generator (Tree Item)
emptyLeafGenerator =
    let
        itemToTree : Item -> Tree Item
        itemToTree item =
            Tree.tree item []
    in
    itemGenerator "" |> Random.map itemToTree



-- NEW INSERTIONS


newChild : FIZ -> Generator FIZ
newChild =
    insertNewHelp (zInsertTreeAtAndFocusIt PrependChild)


newSibling : FIZ -> Generator FIZ
newSibling =
    insertNewHelp (zInsertTreeAtAndFocusIt After)


insertNewHelp insertFunc z =
    let
        insertNewAndChangeFocus newNode =
            insertFunc newNode z
    in
    emptyLeafGenerator
        |> Random.map insertNewAndChangeFocus


moveCursorToItemId : ItemId -> FIZ -> Maybe FIZ
moveCursorToItemId itemId =
    Zipper.firstRoot >> zFindByData (idEq itemId) zGoForward


idEq : ItemId -> Item -> Bool
idEq =
    propEq .id


propEq : (c -> b) -> b -> c -> Bool
propEq func val obj =
    func obj == val


setTitleUnlessBlank : String -> FIZ -> FIZ
setTitleUnlessBlank title =
    \oz ->
        if isBlank title then
            oz

        else
            zMapData (\item -> { item | title = title }) oz


removeIfBlankLeaf : FIZ -> FIZ
removeIfBlankLeaf =
    \zipper ->
        if isBlank (zipper |> zData >> .title) && zIsLeaf zipper then
            Zipper.remove zipper |> Maybe.withDefault zipper

        else
            zipper


isBlank : String -> Bool
isBlank =
    String.trim >> String.isEmpty


currentTitle : FIZ -> String
currentTitle =
    currentItem >> .title


currentItem : FIZ -> Item
currentItem =
    unwrap >> zData


currentId : FIZ -> ItemId
currentId =
    currentItem >> .id


unwrap : FIZ -> FIZ
unwrap z =
    z


moveAfterParent : FIZ -> Maybe FIZ
moveAfterParent =
    relocateBy After up


appendInPreviousSibling : FIZ -> Maybe FIZ
appendInPreviousSibling =
    relocateBy AppendChild left


moveBeforePreviousSibling : FIZ -> Maybe FIZ
moveBeforePreviousSibling =
    relocateBy Before left


appendInPreviousSiblingOfParent : FIZ -> Maybe FIZ
appendInPreviousSiblingOfParent =
    relocateBy AppendChild (up >> Maybe.andThen left)


moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent : FIZ -> Maybe FIZ
moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent =
    Maybe.Extra.oneOf
        [ moveBeforePreviousSibling
        , appendInPreviousSiblingOfParent
        ]


prependInNextSiblingOfParent : FIZ -> Maybe FIZ
prependInNextSiblingOfParent =
    relocateBy PrependChild (up >> Maybe.andThen right)


moveAfterNextSibling : FIZ -> Maybe FIZ
moveAfterNextSibling =
    relocateBy After right


moveAfterNextSiblingOrPrependInNextSiblingOfParent : FIZ -> Maybe FIZ
moveAfterNextSiblingOrPrependInNextSiblingOfParent =
    Maybe.Extra.oneOf
        [ moveAfterNextSibling
        , prependInNextSiblingOfParent
        ]


relocateBy :
    Location
    -> (FIZ -> Maybe FIZ)
    -> FIZ
    -> Maybe FIZ
relocateBy location navigateFunction doc =
    case navigateFunction doc |> Maybe.map currentId of
        Just targetId ->
            relocate location targetId doc

        Nothing ->
            Nothing


relocate : Location -> ItemId -> FIZ -> Maybe FIZ
relocate atLocation targetId =
    unwrap
        >> (\zipper ->
                Zipper.remove zipper
                    |> Maybe.andThen
                        (moveCursorToItemId targetId
                            >> Maybe.map (zInsertTreeAtAndFocusIt atLocation (Zipper.tree zipper))
                        )
           )


toForest : FIZ -> Forest Item
toForest =
    unwrap >> Zipper.firstRoot >> Zipper.forest


currentTree : FIZ -> Tree Item
currentTree =
    unwrap >> Zipper.tree


restructure : (Item -> List c -> c) -> FIZ -> List c
restructure render =
    toForest >> List.map (Tree.restructure identity render)


restructureFocused : (Item -> List c -> c) -> FIZ -> c
restructureFocused render =
    currentTree >> Tree.restructure identity render


left : FIZ -> Maybe FIZ
left =
    Zipper.left


right : FIZ -> Maybe FIZ
right =
    Zipper.right


up : FIZ -> Maybe FIZ
up =
    Zipper.up


down : FIZ -> Maybe FIZ
down =
    Zipper.down


goBackward : FIZ -> Maybe FIZ
goBackward =
    Maybe.Extra.oneOf [ left >> Maybe.map lastDescendant, up ]


lastDescendant : FIZ -> FIZ
lastDescendant zipper =
    case lastChild zipper of
        Nothing ->
            zipper

        Just child ->
            lastDescendant child


lastChild : FIZ -> Maybe FIZ
lastChild =
    down >> Maybe.map (applyWhileJust right)


applyWhileJust : (a -> Maybe a) -> a -> a
applyWhileJust func a =
    case func a of
        Just a2 ->
            applyWhileJust func a2

        Nothing ->
            a


goForward : FIZ -> Maybe FIZ
goForward =
    zGoForward


zNextSiblingOfClosestAncestor : ForestZipper a -> Maybe (ForestZipper a)
zNextSiblingOfClosestAncestor acc =
    case Zipper.up acc of
        Just parentAcc ->
            case Zipper.right parentAcc of
                Just ns ->
                    Just ns

                Nothing ->
                    zNextSiblingOfClosestAncestor parentAcc

        Nothing ->
            Nothing


hasVisibleChildren : FIZ -> Bool
hasVisibleChildren =
    unwrap >> Zipper.tree >> Tree.children >> (not << List.isEmpty)



-- ForestZipper Extra


zInsertTreeAtAndFocusIt : Location -> Tree a -> ForestZipper a -> ForestZipper a
zInsertTreeAtAndFocusIt location =
    let
        helper insertFunc focusFunc node zipper =
            insertFunc node zipper
                |> focusFunc
                |> Maybe.withDefault zipper
    in
    case location of
        Before ->
            helper Zipper.insertLeft Zipper.left

        After ->
            helper Zipper.insertRight Zipper.right

        PrependChild ->
            helper zPrependChild Zipper.down

        AppendChild ->
            helper zAppendChild lastChild


zFindByData : (a -> Bool) -> (ForestZipper a -> Maybe (ForestZipper a)) -> ForestZipper a -> Maybe (ForestZipper a)
zFindByData pred =
    findWithIterator (zData >> pred)


findWithIterator : (a -> Bool) -> (a -> Maybe a) -> a -> Maybe a
findWithIterator pred iterator zipper =
    if pred zipper then
        Just zipper

    else
        case iterator zipper of
            Just nextAcc ->
                findWithIterator pred iterator nextAcc

            Nothing ->
                Nothing


zGoForward =
    Maybe.Extra.oneOf [ Zipper.down, Zipper.right, zNextSiblingOfClosestAncestor ]


zMapData : (a -> a) -> ForestZipper a -> ForestZipper a
zMapData func =
    Zipper.mapTree (Tree.mapData func)


zData : ForestZipper a -> a
zData =
    Zipper.tree >> Tree.data


zPrependChild : Tree a -> ForestZipper a -> ForestZipper a
zPrependChild child =
    Zipper.mapTree (Tree.mapChildren ((::) child))


zAppendChild : Tree a -> ForestZipper a -> ForestZipper a
zAppendChild child =
    Zipper.mapTree (Tree.mapChildren (\children -> children ++ [ child ]))


zIsLeaf : ForestZipper a -> Bool
zIsLeaf =
    Zipper.tree >> Tree.children >> List.isEmpty



{-

   initialItemGenerator : Generator (List Item)
   initialItemGenerator =
               [ "Quick Brown Fox Jumped Over The Lazy Dog"
               , "Take Notes"
               , "Thou shall not experiment with experiments"
               , "Watch Movies"
               , "Run the mill"
               ]
                   |> List.map OutlineDoc.itemGenerator
                   |> Random.Extra.combine
-}
