module OutlineDoc exposing
    ( CandidateLocation(..)
    , Item
    , ItemId
    , OutlineDoc
    , appendInPreviousSibling
    , candidateLocationDecoder
    , candidateLocationEncoder
    , currentId
    , currentTitle
    , decoder
    , encoder
    , focusId
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


candidateLocationEncoder : CandidateLocation -> Value
candidateLocationEncoder candidateLocation =
    let
        encodeHelp : String -> ItemId -> Value
        encodeHelp tagName itemId =
            JE.object
                [ ( "tag", JE.string tagName )
                , ( "id", itemIdEncoder itemId )
                ]
    in
    case candidateLocation of
        Before itemId ->
            encodeHelp "Before" itemId

        After itemId ->
            encodeHelp "After" itemId

        PrependIn itemId ->
            encodeHelp "PrependIn" itemId

        AppendIn itemId ->
            encodeHelp "AppendIn" itemId


candidateLocationDecoder : Decoder CandidateLocation
candidateLocationDecoder =
    let
        decodeHelp : (ItemId -> CandidateLocation) -> Decoder CandidateLocation
        decodeHelp tag =
            JD.field "id" itemIdDecoder
                |> JD.map tag

        tagDecoder : String -> Decoder CandidateLocation
        tagDecoder tag =
            case tag of
                "Before" ->
                    decodeHelp Before

                "After" ->
                    decodeHelp After

                "PrependIn" ->
                    decodeHelp PrependIn

                "AppendIn" ->
                    decodeHelp AppendIn

                _ ->
                    JD.fail ("unknown tag for CandidateLocation: " ++ tag)
    in
    JD.field "tag" JD.string |> JD.andThen tagDecoder



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


type OutlineDoc
    = OutlineDoc (ForestZipper Item)


encoder : OutlineDoc -> Value
encoder (OutlineDoc zipper) =
    Zipper.encoder itemEncoder zipper


decoder : Decoder OutlineDoc
decoder =
    Zipper.decoder itemDecoder |> JD.map OutlineDoc


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


prependNewChild : OutlineDoc -> Generator OutlineDoc
prependNewChild =
    insertNewHelp zPrependChild Zipper.down


insertNewAfter : OutlineDoc -> Generator OutlineDoc
insertNewAfter =
    insertNewHelp Zipper.insertRight Zipper.right


insertNewHelp :
    (Tree Item -> ForestZipper Item -> b)
    -> (b -> Maybe (ForestZipper Item))
    -> OutlineDoc
    -> Generator OutlineDoc
insertNewHelp insertFunc moveFocusFunc (OutlineDoc z) =
    let
        insertNewAndChangeFocus newNode =
            ignoreNothing (insertFunc newNode >> moveFocusFunc) z
    in
    emptyLeafGenerator
        |> Random.map (insertNewAndChangeFocus >> OutlineDoc)


ignoreNothing : (b -> Maybe b) -> b -> b
ignoreNothing func val =
    func val |> Maybe.withDefault val


focusId : ItemId -> OutlineDoc -> Maybe OutlineDoc
focusId itemId =
    let
        findFirst : (a -> Bool) -> ForestZipper a -> Maybe (ForestZipper a)
        findFirst pred =
            Zipper.firstRoot >> find (Zipper.tree >> Tree.data >> pred) Zipper.forward

        find : (a -> Bool) -> (a -> Maybe a) -> a -> Maybe a
        find pred maybeNavFunc zipper =
            if pred zipper then
                Just zipper

            else
                case maybeNavFunc zipper of
                    Just nextAcc ->
                        find pred maybeNavFunc nextAcc

                    Nothing ->
                        Nothing
    in
    mapMaybe (findFirst (propEq .id itemId))


map : (ForestZipper Item -> ForestZipper Item) -> OutlineDoc -> OutlineDoc
map func (OutlineDoc z) =
    func z |> OutlineDoc


mapMaybe : (ForestZipper Item -> Maybe (ForestZipper Item)) -> OutlineDoc -> Maybe OutlineDoc
mapMaybe func (OutlineDoc z) =
    func z |> Maybe.map OutlineDoc


mapRandom : (ForestZipper Item -> Generator (ForestZipper Item)) -> OutlineDoc -> Generator OutlineDoc
mapRandom func (OutlineDoc z) =
    func z |> Random.map OutlineDoc


propEq : (c -> b) -> b -> c -> Bool
propEq func val obj =
    func obj == val


setTitleUnlessBlank : String -> OutlineDoc -> OutlineDoc
setTitleUnlessBlank title =
    map
        (\oz ->
            if isBlank title then
                oz

            else
                zMapData (\item -> { item | title = title }) oz
        )


removeIfBlankLeaf : OutlineDoc -> OutlineDoc
removeIfBlankLeaf =
    map
        (\oz ->
            if isBlank (oz |> zData >> .title) && zIsLeaf oz then
                oz
                    |> withRollback Zipper.remove

            else
                oz
        )


isBlank : String -> Bool
isBlank =
    String.trim >> String.isEmpty


withRollback func oz =
    func oz |> Maybe.withDefault oz


currentTitle : OutlineDoc -> String
currentTitle =
    currentItem >> .title


currentItem : OutlineDoc -> Item
currentItem =
    unwrap >> zData


currentId : OutlineDoc -> ItemId
currentId =
    currentItem >> .id


unwrap : OutlineDoc -> ForestZipper Item
unwrap (OutlineDoc z) =
    z


moveAfterParent : OutlineDoc -> Maybe OutlineDoc
moveAfterParent =
    relocateFocused After up


appendInPreviousSibling : OutlineDoc -> Maybe OutlineDoc
appendInPreviousSibling =
    relocateFocused AppendIn left


moveBeforePreviousSibling : OutlineDoc -> Maybe OutlineDoc
moveBeforePreviousSibling =
    relocateFocused Before left


appendInPreviousSiblingOfParent : OutlineDoc -> Maybe OutlineDoc
appendInPreviousSiblingOfParent =
    relocateFocused AppendIn (up >> Maybe.andThen left)


moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent : OutlineDoc -> Maybe OutlineDoc
moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent =
    Maybe.Extra.oneOf
        [ moveBeforePreviousSibling
        , appendInPreviousSiblingOfParent
        ]


prependInNextSiblingOfParent : OutlineDoc -> Maybe OutlineDoc
prependInNextSiblingOfParent =
    relocateFocused PrependIn (up >> Maybe.andThen right)


moveAfterNextSibling : OutlineDoc -> Maybe OutlineDoc
moveAfterNextSibling =
    relocateFocused After right


moveAfterNextSiblingOrPrependInNextSiblingOfParent : OutlineDoc -> Maybe OutlineDoc
moveAfterNextSiblingOrPrependInNextSiblingOfParent =
    Maybe.Extra.oneOf
        [ moveAfterNextSibling
        , prependInNextSiblingOfParent
        ]


relocateFocused :
    (ItemId -> CandidateLocation)
    -> (OutlineDoc -> Maybe OutlineDoc)
    -> OutlineDoc
    -> Maybe OutlineDoc
relocateFocused candidateLocationFunction navigateFunction doc =
    case navigateFunction doc |> Maybe.map currentId of
        Just id ->
            moveCurrentToCandidateLocation (candidateLocationFunction id) doc

        Nothing ->
            Nothing


moveCurrentToCandidateLocation : CandidateLocation -> OutlineDoc -> Maybe OutlineDoc
moveCurrentToCandidateLocation cl doc =
    moveItemWithIdToCandidateLocationPreservingFocus (currentId doc) cl doc


moveItemWithIdToCandidateLocationPreservingFocus : ItemId -> CandidateLocation -> OutlineDoc -> Maybe OutlineDoc
moveItemWithIdToCandidateLocationPreservingFocus srcItemId candidateLocation =
    let
        moveTo : CandidateLocation -> OutlineDoc -> Maybe OutlineDoc
        moveTo atLocation =
            unwrap
                >> (\zipper ->
                        Zipper.remove zipper
                            |> Maybe.andThen (OutlineDoc >> insertRemovedNodeAtLocation atLocation zipper.center)
                   )

        insertRemovedNodeAtLocation : CandidateLocation -> Tree Item -> OutlineDoc -> Maybe OutlineDoc
        insertRemovedNodeAtLocation atLocation node =
            let
                insertHelp :
                    ItemId
                    -> (Tree Item -> ForestZipper Item -> ForestZipper Item)
                    -> OutlineDoc
                    -> Maybe OutlineDoc
                insertHelp targetItemId func doc =
                    doc
                        |> focusId targetItemId
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
        >> Maybe.andThen (focusId srcItemId)


toForest : OutlineDoc -> Forest Item
toForest =
    unwrap >> Zipper.firstRoot >> Zipper.forest


currentTree : OutlineDoc -> Tree Item
currentTree =
    unwrap >> Zipper.tree


restructure : (Item -> List c -> c) -> OutlineDoc -> List c
restructure render =
    toForest >> List.map (Tree.restructure identity render)


restructureFocused : (Item -> List c -> c) -> OutlineDoc -> c
restructureFocused render =
    currentTree >> Tree.restructure identity render


left : OutlineDoc -> Maybe OutlineDoc
left =
    mapMaybe Zipper.left


right : OutlineDoc -> Maybe OutlineDoc
right =
    mapMaybe Zipper.right


up : OutlineDoc -> Maybe OutlineDoc
up =
    mapMaybe Zipper.up


goBackward : OutlineDoc -> Maybe OutlineDoc
goBackward =
    mapMaybe Zipper.backward


goForward : OutlineDoc -> Maybe OutlineDoc
goForward =
    mapMaybe Zipper.forward


hasVisibleChildren : OutlineDoc -> Bool
hasVisibleChildren =
    unwrap >> Zipper.tree >> Tree.children >> (not << List.isEmpty)



-- ForestZipper Extra


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
