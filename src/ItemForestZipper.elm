module ItemForestZipper exposing
    ( FIZ
    , Item
    , ItemId
    , Location(..)
    , collapse
    , decoder
    , deleteEmpty
    , encoder
    , expand
    , getId
    , getTitle
    , goBackward
    , goDown
    , goForward
    , goLeft
    , goRight
    , goUp
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
    , collapsed : Bool
    }


itemEncoder : Item -> Value
itemEncoder item =
    JE.object
        [ ( "id", itemIdEncoder item.id )
        , ( "title", JE.string item.title )
        , ( "collapsed", JE.bool item.collapsed )
        ]


itemDecoder : Decoder Item
itemDecoder =
    JD.succeed Item
        |> required "id" itemIdDecoder
        |> required "title" JD.string
        |> JD.map2 (|>) (JD.oneOf [ JD.field "collapsed" JD.bool, JD.succeed False ])


type ItemId
    = ItemId String


itemGenerator : String -> Generator Item
itemGenerator title =
    itemIdGenerator
        |> Random.map (\id -> { id = id, title = title, collapsed = False })


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


getTitle : FIZ -> String
getTitle =
    zData >> .title


hasVisibleChildren : FIZ -> Bool
hasVisibleChildren fiz =
    not (zIsLeaf fiz || (zData fiz |> .collapsed))



-- NEW INSERTIONS


newChild : FIZ -> Generator FIZ
newChild =
    insertNewHelp (zInsertTreeAtAndMoveCursor PrependChild)


newSibling : FIZ -> Generator FIZ
newSibling =
    insertNewHelp (zInsertTreeAtAndMoveCursor After)


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


setTitleUnsafe title_ model =
    { model | title = title_ }


nonBlank : String -> Maybe String
nonBlank =
    String.trim
        >> (\trimmedString ->
                if trimmedString == "" then
                    Nothing

                else
                    Just trimmedString
           )


expand : FIZ -> Maybe FIZ
expand fiz =
    if zIsLeaf fiz then
        Nothing

    else
        Just (zMapData (setCollapsedUnsafe False) fiz)


collapse : FIZ -> Maybe FIZ
collapse fiz =
    if zIsLeaf fiz then
        Nothing

    else
        Just (zMapData (setCollapsedUnsafe True) fiz)


setCollapsedUnsafe collapsed model =
    { model | collapsed = collapsed }



-- DELETE NODE


deleteEmpty : FIZ -> Maybe FIZ
deleteEmpty fiz =
    if nonBlank (getTitle fiz) == Nothing && zIsLeaf fiz then
        Zipper.remove fiz

    else
        Nothing



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
relocate relativeLocation targetId zipper =
    let
        removedNode =
            Zipper.tree zipper

        insertHelp =
            zInsertTreeAtAndMoveCursor relativeLocation removedNode
                >> expandAncestors
    in
    Zipper.remove zipper
        |> Maybe.andThen (gotoId targetId >> Maybe.map insertHelp)


expandAncestors : FIZ -> FIZ
expandAncestors =
    Zipper.mapAncestorData (setCollapsedUnsafe False)



-- CORE NAVIGATION


gotoId : ItemId -> FIZ -> Maybe FIZ
gotoId itemId =
    Zipper.firstRoot
        >> zFindByData (idEq itemId)
            (Maybe.Extra.oneOf [ goDown, goRight, gotoNextSiblingOfClosestAncestor ])


goUp : FIZ -> Maybe FIZ
goUp =
    Zipper.up


goDown : FIZ -> Maybe FIZ
goDown fiz =
    if hasVisibleChildren fiz then
        Zipper.down fiz

    else
        Nothing


goLeft : FIZ -> Maybe FIZ
goLeft =
    Zipper.left


goRight : FIZ -> Maybe FIZ
goRight =
    Zipper.right



-- NAVIGATION HELPERS


goForward : FIZ -> Maybe FIZ
goForward =
    Maybe.Extra.oneOf [ goDown, goRight, gotoNextSiblingOfClosestAncestor ]


gotoNextSiblingOfClosestAncestor : FIZ -> Maybe (ForestZipper Item)
gotoNextSiblingOfClosestAncestor fiz =
    case goUp fiz of
        Just parentFIZ ->
            case goRight parentFIZ of
                Just ns ->
                    Just ns

                Nothing ->
                    gotoNextSiblingOfClosestAncestor parentFIZ

        Nothing ->
            Nothing


goBackward : FIZ -> Maybe FIZ
goBackward =
    Maybe.Extra.oneOf [ goLeft >> Maybe.map gotoLastDescendant, goUp ]


gotoLastDescendant : FIZ -> FIZ
gotoLastDescendant zipper =
    case gotoLastChild zipper of
        Nothing ->
            zipper

        Just child ->
            gotoLastDescendant child


gotoLastChild : FIZ -> Maybe FIZ
gotoLastChild =
    goDown >> Maybe.map (applyWhileJust goRight)


idEq : ItemId -> Item -> Bool
idEq =
    propEq .id


propEq : (c -> b) -> b -> c -> Bool
propEq func val obj =
    func obj == val



-- VIEW HELPERS


restructure : (Item -> List c -> c) -> FIZ -> List c
restructure render =
    toForest >> List.map (restructureHelp render)


toForest : FIZ -> Forest Item
toForest =
    Zipper.firstRoot >> Zipper.forest


restructureNodeAtCursor : (Item -> List c -> c) -> FIZ -> c
restructureNodeAtCursor render =
    Zipper.tree >> restructureHelp render


restructureHelp : (Item -> List c -> c) -> Tree Item -> c
restructureHelp render =
    Tree.restructure identity
        (\item children ->
            render item
                (if item.collapsed then
                    []

                 else
                    children
                )
        )



-- ForestZipper Extra


applyWhileJust : (a -> Maybe a) -> a -> a
applyWhileJust func a =
    case func a of
        Just a2 ->
            applyWhileJust func a2

        Nothing ->
            a


zInsertTreeAtAndMoveCursor : Location -> Tree a -> ForestZipper a -> ForestZipper a
zInsertTreeAtAndMoveCursor location =
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
            helper zAppendChild (Zipper.down >> Maybe.map (applyWhileJust Zipper.right))


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
