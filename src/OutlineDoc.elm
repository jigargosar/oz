module OutlineDoc exposing
    ( CandidateLocation
    , OutlineDoc
    , addNew
    , after
    , ancestorIds
    , appendIn
    , before
    , candidateLocationDecoder
    , candidateLocationEncoder
    , collapse
    , currentId
    , currentTitle
    , decoder
    , encoder
    , expand
    , goBackward
    , goForward
    , gotoId
    , gotoParent
    , indent
    , moveDownwards
    , moveUpwards
    , new
    , prependIn
    , relocateTo
    , removeIfBlankLeaf
    , restructureCurrentNode
    , restructureWithContext
    , setTitleUnlessBlank
    , unIndent
    , zoomIn
    , zoomOut
    )

import FIZ as FIZ exposing (FIZ)
import Forest.Zipper as Z exposing (ForestZipper, Location(..))
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Maybe.Extra
import OutlineDoc.Internal exposing (Unwrapped(..), initDoc, initZoomed, open)
import Random exposing (Generator)
import Utils exposing (nonBlank, required)



-- CANDIDATE LOCATION


type CandidateLocation
    = CandidateLocation Location ItemId


candidateLocationEncoder : CandidateLocation -> Value
candidateLocationEncoder (CandidateLocation loc itemId) =
    let
        encodeHelp : String -> Value
        encodeHelp tagName =
            JE.object
                [ ( "tag", JE.string tagName )
                , ( "id", ItemId.itemIdEncoder itemId )
                ]
    in
    case loc of
        Before ->
            encodeHelp "Before"

        After ->
            encodeHelp "After"

        PrependChild ->
            encodeHelp "PrependChild"

        AppendChild ->
            encodeHelp "AppendChild"


candidateLocationDecoder : Decoder CandidateLocation
candidateLocationDecoder =
    let
        decodeHelp : Location -> Decoder CandidateLocation
        decodeHelp tag =
            JD.field "id" ItemId.itemIdDecoder
                |> JD.map (CandidateLocation tag)

        tagDecoder : String -> Decoder CandidateLocation
        tagDecoder tag =
            case tag of
                "Before" ->
                    decodeHelp Before

                "After" ->
                    decodeHelp After

                "PrependChild" ->
                    decodeHelp PrependChild

                "AppendChild" ->
                    decodeHelp AppendChild

                _ ->
                    JD.fail ("unknown tag for CandidateLocation: " ++ tag)
    in
    JD.field "tag" JD.string |> JD.andThen tagDecoder


before : ItemId -> CandidateLocation
before =
    CandidateLocation Before


after : ItemId -> CandidateLocation
after =
    CandidateLocation After


prependIn : ItemId -> CandidateLocation
prependIn =
    CandidateLocation PrependChild


appendIn : ItemId -> CandidateLocation
appendIn =
    CandidateLocation AppendChild



-- ITEM


type alias Item =
    FIZ.Item



-- DOC MODEL


type alias OutlineDoc =
    OutlineDoc.Internal.OutlineDoc


new : Generator OutlineDoc
new =
    FIZ.new |> Random.map initDoc


zoomIn : OutlineDoc -> Maybe OutlineDoc
zoomIn doc =
    case open doc of
        Doc z ->
            Z.childrenAsZipper z |> Maybe.map (initZoomed z)

        Zoomed pz z ->
            Z.merge z pz
                |> (\newPZ -> Z.childrenAsZipper newPZ |> Maybe.map (initZoomed newPZ))


zoomOut : OutlineDoc -> Maybe OutlineDoc
zoomOut doc =
    case open doc of
        Doc _ ->
            Nothing

        Zoomed pz z ->
            case Z.transferOneLevelTo z pz of
                ( newZ, Just newPZ ) ->
                    Just (initZoomed newPZ (FIZ.expandAncestors newZ))

                ( newZ, Nothing ) ->
                    Just (initDoc newZ)


encoder : OutlineDoc -> Value
encoder doc =
    case open doc of
        Doc z ->
            FIZ.encoder z

        Zoomed pz z ->
            JE.object [ ( "tag", JE.string "Zoomed" ), ( "pz", FIZ.encoder pz ), ( "z", FIZ.encoder z ) ]


decoder : Decoder OutlineDoc
decoder =
    JD.oneOf
        [ FIZ.decoder |> JD.map initDoc
        , JD.succeed initZoomed |> required "pz" FIZ.decoder |> required "z" FIZ.decoder
        ]


map : (FIZ -> FIZ) -> OutlineDoc -> OutlineDoc
map func doc =
    case open doc of
        Doc z ->
            initDoc (func z)

        Zoomed pz z ->
            func z |> initZoomed pz


mapMaybe : (FIZ -> Maybe FIZ) -> OutlineDoc -> Maybe OutlineDoc
mapMaybe func doc =
    case open doc of
        Doc z ->
            func z |> Maybe.map initDoc

        Zoomed pz z ->
            func z |> Maybe.map (initZoomed pz)


unwrap : OutlineDoc -> FIZ
unwrap doc =
    case open doc of
        Doc z ->
            z

        Zoomed _ z ->
            z



-- Getters


currentTitle : OutlineDoc -> String
currentTitle =
    unwrap >> zTitle


zTitle : FIZ -> String
zTitle =
    Z.data >> .title


ancestorIds : OutlineDoc -> List ItemId
ancestorIds =
    unwrap >> Z.ancestors >> List.map .id


currentId : OutlineDoc -> ItemId
currentId =
    unwrap >> zId


zId : FIZ -> ItemId
zId =
    Z.data >> .id



-- NEW INSERTIONS


addNew : OutlineDoc -> Generator OutlineDoc
addNew doc =
    case open doc of
        Doc z ->
            z |> FIZ.addNew >> Random.map initDoc

        Zoomed pz z ->
            z |> FIZ.addNew >> Random.map (initZoomed pz)



-- Update Node


setTitleUnlessBlank : String -> OutlineDoc -> OutlineDoc
setTitleUnlessBlank title =
    map
        (FIZ.setTitle title |> ignoreNothing)


ignoreNothing f v =
    f v |> Maybe.withDefault v


expand : OutlineDoc -> Maybe OutlineDoc
expand =
    mapMaybe FIZ.expand


collapse : OutlineDoc -> Maybe OutlineDoc
collapse =
    mapMaybe FIZ.collapse


removeIfBlankLeaf : OutlineDoc -> OutlineDoc
removeIfBlankLeaf =
    map (deleteEmpty |> ignoreNothing)


deleteEmpty : FIZ -> Maybe FIZ
deleteEmpty z =
    if nonBlank (zTitle z) == Nothing && Z.isLeaf z then
        Z.remove z

    else
        Nothing



-- MOVE CURSOR


gotoId : ItemId -> OutlineDoc -> Maybe OutlineDoc
gotoId itemId =
    mapMaybe (FIZ.gotoId itemId)


goBackward : OutlineDoc -> Maybe OutlineDoc
goBackward =
    mapMaybe FIZ.goBackward


goForward : OutlineDoc -> Maybe OutlineDoc
goForward =
    mapMaybe FIZ.goForward


gotoParent : OutlineDoc -> Maybe OutlineDoc
gotoParent =
    mapMaybe FIZ.goUp



-- MOVE NODE


relocateTo : CandidateLocation -> OutlineDoc -> Maybe OutlineDoc
relocateTo (CandidateLocation loc itemId) =
    mapMaybe (zRelocate loc itemId)


unIndent : OutlineDoc -> Maybe OutlineDoc
unIndent =
    -- moveAfterParent
    mapMaybe (zRelocateBy After FIZ.goUp)


indent : OutlineDoc -> Maybe OutlineDoc
indent =
    -- appendInPreviousSibling
    mapMaybe (zRelocateBy AppendChild FIZ.goLeft)


moveUpwards : OutlineDoc -> Maybe OutlineDoc
moveUpwards =
    -- moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent
    Maybe.Extra.oneOf
        [ mapMaybe (zRelocateBy Before FIZ.goLeft)
        , mapMaybe (zRelocateBy AppendChild (FIZ.goUp >> Maybe.andThen FIZ.goLeft))
        ]


moveDownwards : OutlineDoc -> Maybe OutlineDoc
moveDownwards =
    -- moveAfterNextSiblingOrPrependInNextSiblingOfParent
    mapMaybe
        (Maybe.Extra.oneOf
            [ zRelocateBy After FIZ.goRight
            , zRelocateBy PrependChild (FIZ.goUp >> Maybe.andThen FIZ.goRight)
            ]
        )


zRelocateBy :
    Location
    -> (FIZ -> Maybe FIZ)
    -> FIZ
    -> Maybe FIZ
zRelocateBy loc findTargetFunc doc =
    case findTargetFunc doc |> Maybe.map zId of
        Just targetId ->
            FIZ.relocate loc targetId doc

        Nothing ->
            Nothing


zRelocate : Location -> ItemId -> FIZ -> Maybe FIZ
zRelocate relativeLocation targetId zipper =
    let
        removedNode =
            Z.tree zipper

        insertHelp =
            Z.insertAndGoto relativeLocation removedNode
                >> FIZ.expandAncestors
    in
    Z.remove zipper
        |> Maybe.andThen (FIZ.gotoId targetId >> Maybe.map insertHelp)



-- VIEW


wrapRender render a b c =
    render ( { id = a.id, title = a.title, collapsed = c }, List.map .id b )


restructureWithContext render =
    unwrap >> FIZ.restructureWithContext (wrapRender render)


restructureCurrentNode render =
    unwrap >> FIZ.restructureCursorWithContext (wrapRender render)
