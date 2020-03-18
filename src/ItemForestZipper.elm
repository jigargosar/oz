module ItemForestZipper exposing
    ( FIZ
    , Item
    , Location(..)
    , addNew
    , ancestorIds
    , collapse
    , decoder
    , deleteEmpty
    , encoder
    , expand
    , getId
    , getTitle
    , goBackward
    , goForward
    , goLeft
    , goRight
    , goUp
    , gotoId
    , new
    , relocate
    , relocateBy
    , restructureCursorWithContext
    , restructureWithContext
    , setTitle
    )

import CollapseState exposing (CollapseState)
import Forest.Tree as Tree exposing (Forest, Tree)
import Forest.Zipper as Zipper exposing (ForestZipper)
import ItemId exposing (ItemId)
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


itemGenerator : String -> Generator Item
itemGenerator title =
    ItemId.itemIdGenerator
        |> Random.map (\id -> { id = id, title = title, collapsed = False })


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


new : Generator FIZ
new =
    emptyLeafGenerator
        |> Random.map Zipper.fromTree


getId : FIZ -> ItemId
getId =
    Zipper.data >> .id


getTitle : FIZ -> String
getTitle =
    Zipper.data >> .title


ancestorIds : FIZ -> List ItemId
ancestorIds =
    Zipper.ancestors >> List.map .id


hasVisibleChildren : FIZ -> Bool
hasVisibleChildren fiz =
    not (Zipper.isLeaf fiz || (Zipper.data fiz |> .collapsed))



-- NEW INSERTIONS


addNew : FIZ -> Generator FIZ
addNew fiz =
    let
        insertNewHelper node =
            if hasVisibleChildren fiz then
                Zipper.prependChildGo node fiz

            else
                Zipper.insertRightGo node fiz
    in
    emptyLeafGenerator
        |> Random.map insertNewHelper



-- UPDATE NODE


setTitle : String -> FIZ -> Maybe FIZ
setTitle rawTitle fiz =
    case nonBlank rawTitle of
        Just title ->
            Just (Zipper.mapData (setTitleUnsafe title) fiz)

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
    if Zipper.isLeaf fiz then
        Nothing

    else
        Just (Zipper.mapData (setCollapsedUnsafe False) fiz)


collapse : FIZ -> Maybe FIZ
collapse fiz =
    if Zipper.isLeaf fiz then
        Nothing

    else
        Just (Zipper.mapData (setCollapsedUnsafe True) fiz)


setCollapsedUnsafe collapsed model =
    { model | collapsed = collapsed }



-- DELETE NODE


deleteEmpty : FIZ -> Maybe FIZ
deleteEmpty fiz =
    if nonBlank (getTitle fiz) == Nothing && Zipper.isLeaf fiz then
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
            zInsertAndGoto relativeLocation removedNode
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


restructureWithContext : (Item -> List Item -> CollapseState -> List b -> b) -> ForestZipper Item -> List b
restructureWithContext render =
    Zipper.restructure
        (\fiz children ->
            let
                item =
                    Zipper.data fiz
            in
            render
                item
                (Zipper.ancestors fiz)
                (if List.isEmpty children then
                    CollapseState.NoChildren

                 else if item.collapsed then
                    CollapseState.Collapsed

                 else
                    CollapseState.Expanded
                )
                (if item.collapsed then
                    []

                 else
                    children
                )
        )


restructureCursorWithContext : (Item -> List Item -> CollapseState -> List b -> b) -> ForestZipper Item -> List b
restructureCursorWithContext render =
    Zipper.tree
        >> Zipper.fromTree
        >> restructureWithContext render



-- ForestZipper Extra


applyWhileJust : (a -> Maybe a) -> a -> a
applyWhileJust func a =
    case func a of
        Just a2 ->
            applyWhileJust func a2

        Nothing ->
            a


zInsertAndGoto : Location -> Tree a -> ForestZipper a -> ForestZipper a
zInsertAndGoto location =
    case location of
        Before ->
            Zipper.insertLeftGo

        After ->
            Zipper.insertRightGo

        PrependChild ->
            Zipper.prependChildGo

        AppendChild ->
            Zipper.appendChildGo


zFindByData : (a -> Bool) -> (ForestZipper a -> Maybe (ForestZipper a)) -> ForestZipper a -> Maybe (ForestZipper a)
zFindByData pred =
    findWithIterator (Zipper.data >> pred)


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
