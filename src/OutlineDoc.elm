module OutlineDoc exposing
    ( CandidateLocation(..)
    , Item
    , ItemId
    , OutlineDoc
    , OutlineNode
    , addNewLine
    , appendInPreviousSibling
    , appendInPreviousSiblingOfParent
    , candidateLocationDecoder
    , candidateLocationEncoder
    , currentId
    , currentTitle
    , decoder
    , encoder
    , focusId
    , goBackward
    , goForward
    , itemIdDecoder
    , itemIdEncoder
    , moveAfterNextSibling
    , moveAfterParent
    , moveBeforePreviousSibling
    , moveCurrentToCandidateLocation
    , prependInNextSiblingOfParent
    , removeIfBlankLeaf
    , restructure
    , restructureFocused
    , setTitleUnlessBlank
    )

import Forest
import Forest.Tree as Tree exposing (Tree)
import Forest.Zipper as Zipper exposing (ForestZipper)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
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


type alias Item =
    { id : ItemId
    , title : String
    }



-- ITEM HELPERS


type ItemId
    = ItemId String


itemGenerator : String -> Generator Item
itemGenerator title =
    itemIdGen
        |> Random.map (\id -> { id = id, title = title })


itemIdGen : Generator ItemId
itemIdGen =
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


type alias OutlineNode =
    Tree Item


type alias OutlineForest =
    List OutlineNode


type OutlineDoc
    = OutlineDoc (ForestZipper Item)


encoder : OutlineDoc -> Value
encoder (OutlineDoc outlineZipper) =
    JE.object
        [ ( "leftReversed", JE.list itemTreeEncoder outlineZipper.leftReversed )
        , ( "center", itemTreeEncoder outlineZipper.center )
        , ( "right_", JE.list itemTreeEncoder outlineZipper.right_ )
        , ( "crumbs", JE.list crumbEncoder outlineZipper.crumbs )
        ]


itemTreeEncoder : Tree Item -> Value
itemTreeEncoder tree =
    JE.object
        [ ( "item", itemEncoder (Tree.data tree) )
        , ( "children", JE.list itemTreeEncoder (Tree.children tree) )
        ]


itemEncoder : Item -> Value
itemEncoder item =
    JE.object
        [ ( "id", itemIdEncoder item.id )
        , ( "title", JE.string item.title )
        ]


crumbEncoder : Zipper.Crumb Item -> Value
crumbEncoder crumb =
    JE.object
        [ ( "leftReversed", JE.list itemTreeEncoder crumb.leftReversed )
        , ( "datum", itemEncoder crumb.datum )
        , ( "right_", JE.list itemTreeEncoder crumb.right_ )
        ]


required fieldName decoder_ =
    JD.map2 (|>) (JD.field fieldName decoder_)


decoder : Decoder OutlineDoc
decoder =
    JD.succeed ForestZipper
        |> required "leftReversed" (JD.list treeDecoder)
        |> required "center" treeDecoder
        |> required "right_" (JD.list treeDecoder)
        |> required "crumbs" (JD.list crumbDecoder)
        |> JD.map OutlineDoc


treeDecoder : Decoder OutlineNode
treeDecoder =
    JD.succeed Tree.tree
        |> required "item" itemDecoder
        |> required "children" (JD.list (JD.lazy (\_ -> treeDecoder)))


itemDecoder : Decoder Item
itemDecoder =
    JD.succeed Item
        |> required "id" itemIdDecoder
        |> required "title" JD.string


crumbDecoder : Decoder (Zipper.Crumb Item)
crumbDecoder =
    JD.succeed Zipper.Crumb
        |> required "leftReversed" (JD.list treeDecoder)
        |> required "datum" itemDecoder
        |> required "right_" (JD.list treeDecoder)


insertNew : Item -> OutlineDoc -> OutlineDoc
insertNew item =
    map (Zipper.prependChildAndFocus (Tree.leaf item))


addNewLine : String -> OutlineDoc -> Generator OutlineDoc
addNewLine title doc =
    itemGenerator title
        |> Random.map (\item -> insertNew item doc)


focusId : ItemId -> OutlineDoc -> Maybe OutlineDoc
focusId itemId =
    mapMaybe (Zipper.findFirst (propEq .id itemId))


map : (ForestZipper Item -> ForestZipper Item) -> OutlineDoc -> OutlineDoc
map func (OutlineDoc z) =
    func z |> OutlineDoc


mapMaybe : (ForestZipper Item -> Maybe (ForestZipper Item)) -> OutlineDoc -> Maybe OutlineDoc
mapMaybe func (OutlineDoc z) =
    func z |> Maybe.map OutlineDoc


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
                Zipper.mapData (\item -> { item | title = title }) oz
        )


zTitle : ForestZipper Item -> String
zTitle =
    Zipper.data >> .title


removeIfBlankLeaf : OutlineDoc -> OutlineDoc
removeIfBlankLeaf =
    map
        (\oz ->
            if isBlank (zTitle oz) && Zipper.isLeaf oz then
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
    unwrap >> Zipper.data


currentId : OutlineDoc -> ItemId
currentId =
    currentItem >> .id


unwrap : OutlineDoc -> ForestZipper Item
unwrap (OutlineDoc z) =
    z


moveAfterParent : OutlineDoc -> Maybe OutlineDoc
moveAfterParent =
    relocateFocusedBy up After


appendInPreviousSibling : OutlineDoc -> Maybe OutlineDoc
appendInPreviousSibling =
    relocateFocusedBy left AppendIn


moveBeforePreviousSibling : OutlineDoc -> Maybe OutlineDoc
moveBeforePreviousSibling =
    relocateFocusedBy left Before


appendInPreviousSiblingOfParent : OutlineDoc -> Maybe OutlineDoc
appendInPreviousSiblingOfParent =
    relocateFocusedBy (up >> Maybe.andThen left) AppendIn


prependInNextSiblingOfParent : OutlineDoc -> Maybe OutlineDoc
prependInNextSiblingOfParent =
    relocateFocusedBy (up >> Maybe.andThen right) PrependIn


moveAfterNextSibling : OutlineDoc -> Maybe OutlineDoc
moveAfterNextSibling =
    relocateFocusedBy right After


relocateFocusedBy :
    (OutlineDoc -> Maybe OutlineDoc)
    -> (ItemId -> CandidateLocation)
    -> OutlineDoc
    -> Maybe OutlineDoc
relocateFocusedBy navigateFunction candidateLocationFunction doc =
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

        insertRemovedNodeAtLocation : CandidateLocation -> OutlineNode -> OutlineDoc -> Maybe OutlineDoc
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
                    insertHelp itemId Zipper.insertAndGoRight

                PrependIn itemId ->
                    insertHelp itemId Zipper.prependChildAndFocus

                AppendIn itemId ->
                    insertHelp itemId Zipper.appendChild
    in
    moveTo candidateLocation
        >> Maybe.andThen (focusId srcItemId)


toForest_ : OutlineDoc -> Tree.Forest Item
toForest_ =
    unwrap >> Zipper.toRootForest


currentTree_ : OutlineDoc -> Tree Item
currentTree_ =
    unwrap >> Zipper.getTree



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


restructure : (Item -> List c -> c) -> OutlineDoc -> List c
restructure render =
    toForest_ >> Forest.restructure identity render


restructureFocused : (Item -> List c -> c) -> OutlineDoc -> c
restructureFocused render =
    currentTree_ >> Tree.restructure identity render


goBackward : OutlineDoc -> Maybe OutlineDoc
goBackward =
    mapMaybe Zipper.backward


left : OutlineDoc -> Maybe OutlineDoc
left =
    mapMaybe Zipper.left


right : OutlineDoc -> Maybe OutlineDoc
right =
    mapMaybe Zipper.right


up : OutlineDoc -> Maybe OutlineDoc
up =
    mapMaybe Zipper.up


goForward : OutlineDoc -> Maybe OutlineDoc
goForward =
    mapMaybe Zipper.forward
