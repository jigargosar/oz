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
    , insertNewAfter
    , itemIdDecoder
    , itemIdEncoder
    , moveAfterNextSiblingOrPrependInNextSiblingOfParent
    , moveAfterParent
    , moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent
    , moveCurrentToCandidateLocation
    , moveFocusToItemId
    , prependNewChild
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


type CandidateLocation
    = Before ItemId
    | After ItemId
    | PrependIn ItemId
    | AppendIn ItemId



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



-- DOC MODEL


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



-- NEW INSERTIONS


prependNewChild : FIZ -> Generator FIZ
prependNewChild =
    insertNewHelp zPrependChild Zipper.down


insertNewAfter : FIZ -> Generator FIZ
insertNewAfter =
    insertNewHelp Zipper.insertRight Zipper.right


insertNewHelp :
    (Tree Item -> ForestZipper Item -> b)
    -> (b -> Maybe (ForestZipper Item))
    -> FIZ
    -> Generator FIZ
insertNewHelp insertFunc moveFocusFunc z =
    let
        insertNewAndChangeFocus newNode =
            (insertFunc newNode >> moveFocusFunc) z |> Maybe.withDefault z
    in
    emptyLeafGenerator
        |> Random.map insertNewAndChangeFocus


moveFocusToItemId : ItemId -> FIZ -> Maybe FIZ
moveFocusToItemId itemId =
    mapMaybe (Zipper.firstRoot >> zFindByData (idEq itemId) zGoForward)


idEq : ItemId -> Item -> Bool
idEq =
    propEq .id


map : (ForestZipper Item -> ForestZipper Item) -> FIZ -> FIZ
map func z =
    func z


mapMaybe : (ForestZipper Item -> Maybe (ForestZipper Item)) -> FIZ -> Maybe FIZ
mapMaybe func z =
    func z


propEq : (c -> b) -> b -> c -> Bool
propEq func val obj =
    func obj == val


setTitleUnlessBlank : String -> FIZ -> FIZ
setTitleUnlessBlank title =
    map
        (\oz ->
            if isBlank title then
                oz

            else
                zMapData (\item -> { item | title = title }) oz
        )


removeIfBlankLeaf : FIZ -> FIZ
removeIfBlankLeaf =
    map
        (\zipper ->
            if isBlank (zipper |> zData >> .title) && zIsLeaf zipper then
                Zipper.remove zipper |> Maybe.withDefault zipper

            else
                zipper
        )


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


unwrap : FIZ -> ForestZipper Item
unwrap z =
    z


moveAfterParent : FIZ -> Maybe FIZ
moveAfterParent =
    relocateFocused After up


appendInPreviousSibling : FIZ -> Maybe FIZ
appendInPreviousSibling =
    relocateFocused AppendIn left


moveBeforePreviousSibling : FIZ -> Maybe FIZ
moveBeforePreviousSibling =
    relocateFocused Before left


appendInPreviousSiblingOfParent : FIZ -> Maybe FIZ
appendInPreviousSiblingOfParent =
    relocateFocused AppendIn (up >> Maybe.andThen left)


moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent : FIZ -> Maybe FIZ
moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent =
    Maybe.Extra.oneOf
        [ moveBeforePreviousSibling
        , appendInPreviousSiblingOfParent
        ]


prependInNextSiblingOfParent : FIZ -> Maybe FIZ
prependInNextSiblingOfParent =
    relocateFocused PrependIn (up >> Maybe.andThen right)


moveAfterNextSibling : FIZ -> Maybe FIZ
moveAfterNextSibling =
    relocateFocused After right


moveAfterNextSiblingOrPrependInNextSiblingOfParent : FIZ -> Maybe FIZ
moveAfterNextSiblingOrPrependInNextSiblingOfParent =
    Maybe.Extra.oneOf
        [ moveAfterNextSibling
        , prependInNextSiblingOfParent
        ]


relocateFocused :
    (ItemId -> CandidateLocation)
    -> (FIZ -> Maybe FIZ)
    -> FIZ
    -> Maybe FIZ
relocateFocused candidateLocationFunction navigateFunction doc =
    case navigateFunction doc |> Maybe.map currentId of
        Just id ->
            moveCurrentToCandidateLocation (candidateLocationFunction id) doc

        Nothing ->
            Nothing


moveCurrentToCandidateLocation : CandidateLocation -> FIZ -> Maybe FIZ
moveCurrentToCandidateLocation cl doc =
    moveItemWithIdToCandidateLocationPreservingFocus (currentId doc) cl doc


moveItemWithIdToCandidateLocationPreservingFocus : ItemId -> CandidateLocation -> FIZ -> Maybe FIZ
moveItemWithIdToCandidateLocationPreservingFocus srcItemId candidateLocation =
    let
        moveTo : CandidateLocation -> FIZ -> Maybe FIZ
        moveTo atLocation =
            unwrap
                >> (\zipper ->
                        Zipper.remove zipper
                            |> Maybe.andThen (insertRemovedNodeAtLocation atLocation zipper.center)
                   )

        insertRemovedNodeAtLocation : CandidateLocation -> Tree Item -> FIZ -> Maybe FIZ
        insertRemovedNodeAtLocation atLocation node =
            let
                insertHelp :
                    ItemId
                    -> (Tree Item -> ForestZipper Item -> ForestZipper Item)
                    -> FIZ
                    -> Maybe FIZ
                insertHelp targetItemId func doc =
                    doc
                        |> moveFocusToItemId targetItemId
                        >> Maybe.map (map (func node))
            in
            case atLocation of
                Before itemId ->
                    insertHelp itemId Zipper.insertLeft

                After itemId ->
                    insertHelp itemId Zipper.insertRight

                PrependIn itemId ->
                    insertHelp itemId zPrependChild

                AppendIn itemId ->
                    insertHelp itemId zAppendChild
    in
    moveTo candidateLocation
        >> Maybe.andThen (moveFocusToItemId srcItemId)


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
    mapMaybe Zipper.left


right : FIZ -> Maybe FIZ
right =
    mapMaybe Zipper.right


up : FIZ -> Maybe FIZ
up =
    mapMaybe Zipper.up


down : FIZ -> Maybe FIZ
down =
    mapMaybe Zipper.down


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
    mapMaybe zGoForward


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
