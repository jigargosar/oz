module OutlineDoc exposing
    ( CandidateLocation(..)
    , Item
    , ItemId
    , OutlineDoc
    , OutlineNode
    , currentTree
    , decoder
    , encoder
    , gotoItemId
    , itemGenerator
    , itemIdDecoder
    , itemIdEncoder
    , moveItemWithIdToCandidateLocation
    , ozId
    , ozItem
    , ozNew
    , ozSetTitleUnlessBlankOrRemoveIfBlankLeaf
    , ozTitle
    , toForest
    )

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


ozNew : Item -> OutlineDoc -> OutlineDoc
ozNew item =
    map (Zipper.prependChildAndFocus (Tree.leaf item))


gotoItemId : ItemId -> OutlineDoc -> Maybe OutlineDoc
gotoItemId itemId =
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


ozSetTitleUnlessBlankOrRemoveIfBlankLeaf : String -> OutlineDoc -> OutlineDoc
ozSetTitleUnlessBlankOrRemoveIfBlankLeaf title =
    map
        (\oz ->
            if isBlank title then
                if Zipper.isLeaf oz then
                    oz
                        |> withRollback Zipper.remove

                else
                    oz

            else
                Zipper.mapData (\item -> { item | title = title }) oz
        )


isBlank : String -> Bool
isBlank =
    String.trim >> String.isEmpty


withRollback func oz =
    func oz |> Maybe.withDefault oz


ozTitle : OutlineDoc -> String
ozTitle =
    ozItem >> .title


ozItem : OutlineDoc -> Item
ozItem =
    unwrap >> Zipper.data


ozId : OutlineDoc -> ItemId
ozId =
    ozItem >> .id


unwrap : OutlineDoc -> ForestZipper Item
unwrap (OutlineDoc z) =
    z


moveItemWithIdToCandidateLocation : ItemId -> CandidateLocation -> OutlineDoc -> Maybe OutlineDoc
moveItemWithIdToCandidateLocation srcItemId candidateLocation =
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
                insertHelp targetItemId func =
                    gotoItemId targetItemId >> Maybe.map (map (func node))
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
    gotoItemId srcItemId
        >> Maybe.andThen (moveTo candidateLocation)


toForest : OutlineDoc -> Tree.Forest Item
toForest =
    unwrap >> Zipper.toRootForest


currentTree : OutlineDoc -> Tree Item
currentTree =
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
