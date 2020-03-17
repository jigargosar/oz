module ItemForestZipper exposing
    ( FIZ
    , Item
    , ItemId
    , decoder
    , encoder
    , getId
    , goBackward
    , goForward
    , gotoId
    , hasVisibleChildren
    , itemIdDecoder
    , itemIdEncoder
    , newChild
    , newSibling
    , relocate
    , relocateBy
    , restructure
    , restructureNodeAtCursor
    , setTitle
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
    ForestZipper Item


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


getId : FIZ -> ItemId
getId =
    zData >> .id


hasVisibleChildren : FIZ -> Bool
hasVisibleChildren =
    Zipper.tree >> Tree.children >> (not << List.isEmpty)



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



-- UPDATE NODE


setTitle : String -> FIZ -> Maybe FIZ
setTitle rawTitle fiz =
    case nonBlank rawTitle of
        Just title ->
            Just (zMapData (setTitleUnsafe title) fiz)

        Nothing ->
            Nothing


setTitleUnsafe title model =
    { model | title = title }


nonBlank : String -> Maybe String
nonBlank =
    String.trim
        >> (\trimmedString ->
                if trimmedString == "" then
                    Nothing

                else
                    Just trimmedString
           )



-- MOVE NODE AT CURSOR


relocateBy :
    Location
    -> (FIZ -> Maybe FIZ)
    -> FIZ
    -> Maybe FIZ
relocateBy relativeLocation findTargetFunc doc =
    case findTargetFunc doc |> Maybe.map getId of
        Just targetId ->
            relocate relativeLocation targetId doc

        Nothing ->
            Nothing


relocate : Location -> ItemId -> FIZ -> Maybe FIZ
relocate relativeLocation targetId =
    \zipper ->
        Zipper.remove zipper
            |> Maybe.andThen
                (gotoId targetId
                    >> Maybe.map (zInsertTreeAtAndFocusIt relativeLocation (Zipper.tree zipper))
                )



-- NAVIGATION


gotoId : ItemId -> FIZ -> Maybe FIZ
gotoId itemId =
    Zipper.firstRoot >> zFindByData (idEq itemId) zGoForward


goForward : FIZ -> Maybe FIZ
goForward =
    zGoForward


goBackward : FIZ -> Maybe FIZ
goBackward =
    Maybe.Extra.oneOf [ left >> Maybe.map lastDescendant, up ]



-- NAVIGATION HELPERS


idEq : ItemId -> Item -> Bool
idEq =
    propEq .id


propEq : (c -> b) -> b -> c -> Bool
propEq func val obj =
    func obj == val


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


lastDescendant : FIZ -> FIZ
lastDescendant zipper =
    case zLastChild zipper of
        Nothing ->
            zipper

        Just child ->
            lastDescendant child



-- VIEW HELPERS


restructure : (Item -> List c -> c) -> FIZ -> List c
restructure render =
    toForest >> List.map (Tree.restructure identity render)


toForest : FIZ -> Forest Item
toForest =
    Zipper.firstRoot >> Zipper.forest


restructureNodeAtCursor : (Item -> List c -> c) -> FIZ -> c
restructureNodeAtCursor render =
    Zipper.tree >> Tree.restructure identity render



-- ForestZipper Extra


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


zLastChild : ForestZipper a -> Maybe (ForestZipper a)
zLastChild =
    Zipper.down >> Maybe.map (applyWhileJust Zipper.right)


applyWhileJust : (a -> Maybe a) -> a -> a
applyWhileJust func a =
    case func a of
        Just a2 ->
            applyWhileJust func a2

        Nothing ->
            a


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
            helper zAppendChild zLastChild


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
