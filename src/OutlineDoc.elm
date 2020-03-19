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
    , collapseOrNavParent
    , currentId
    , currentTitle
    , decoder
    , encoder
    , expand
    , expandOrGoForward
    , goBackward
    , goForward
    , gotoId
    , indent
    , moveAfterNextSiblingOrPrependInNextSiblingOfParent
    , moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent
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

import Dict exposing (Dict)
import FIZ as FIZ exposing (FIZ, Location(..))
import Forest.Zipper as Z
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Maybe.Extra
import OutlineDoc.Internal exposing (Unwrapped(..), initDoc, initZoomed, open)
import Random exposing (Generator)
import Tree as T
import Utils exposing (required)



-- CANDIDATE LOCATION


type CandidateLocation
    = CandidateLocation FIZ.Location ItemId


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



-- NEW INSERTIONS


addNew : OutlineDoc -> Generator OutlineDoc
addNew doc =
    case open doc of
        Doc z ->
            z |> FIZ.addNew >> Random.map initDoc

        Zoomed pz z ->
            z |> FIZ.addNew >> Random.map (initZoomed pz)


gotoId : ItemId -> OutlineDoc -> Maybe OutlineDoc
gotoId itemId =
    mapMaybe (FIZ.gotoId itemId)


setTitleUnlessBlank : String -> OutlineDoc -> OutlineDoc
setTitleUnlessBlank title =
    map
        (FIZ.setTitle title |> ignoreNothing)


ignoreNothing f v =
    f v |> Maybe.withDefault v


expand : OutlineDoc -> Maybe OutlineDoc
expand =
    mapMaybe FIZ.expand


expandOrGoForward : OutlineDoc -> Maybe OutlineDoc
expandOrGoForward =
    mapMaybe (Maybe.Extra.oneOf [ FIZ.expand, FIZ.goForward ])


collapse : OutlineDoc -> Maybe OutlineDoc
collapse =
    mapMaybe FIZ.collapse


collapseOrNavParent : OutlineDoc -> Maybe OutlineDoc
collapseOrNavParent =
    mapMaybe (Maybe.Extra.oneOf [ FIZ.collapse, FIZ.goUp ])


removeIfBlankLeaf : OutlineDoc -> OutlineDoc
removeIfBlankLeaf =
    map
        (FIZ.deleteEmpty |> ignoreNothing)


currentTitle : OutlineDoc -> String
currentTitle =
    unwrap >> FIZ.getTitle


ancestorIds =
    unwrap >> FIZ.ancestorIds


currentId : OutlineDoc -> ItemId
currentId =
    unwrap >> FIZ.getId


relocateBy : FIZ.Location -> (FIZ -> Maybe FIZ) -> OutlineDoc -> Maybe OutlineDoc
relocateBy a b =
    mapMaybe (FIZ.relocateBy a b)


unIndent : OutlineDoc -> Maybe OutlineDoc
unIndent =
    -- moveAfterParent
    relocateBy FIZ.After FIZ.goUp


indent : OutlineDoc -> Maybe OutlineDoc
indent =
    -- appendInPreviousSibling
    relocateBy FIZ.AppendChild FIZ.goLeft


moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent : OutlineDoc -> Maybe OutlineDoc
moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent =
    Maybe.Extra.oneOf
        [ relocateBy FIZ.Before FIZ.goLeft
        , relocateBy FIZ.AppendChild (FIZ.goUp >> Maybe.andThen FIZ.goLeft)
        ]


moveAfterNextSiblingOrPrependInNextSiblingOfParent : OutlineDoc -> Maybe OutlineDoc
moveAfterNextSiblingOrPrependInNextSiblingOfParent =
    Maybe.Extra.oneOf
        [ relocateBy FIZ.After FIZ.goRight
        , relocateBy FIZ.PrependChild (FIZ.goUp >> Maybe.andThen FIZ.goRight)
        ]


relocateTo : CandidateLocation -> OutlineDoc -> Maybe OutlineDoc
relocateTo (CandidateLocation loc itemId) =
    mapMaybe (FIZ.relocate loc itemId)


wrapRender render a b c =
    render ( { id = a.id, title = a.title, collapsed = c }, List.map .id b )


restructureWithContext render =
    unwrap >> FIZ.restructureWithContext (wrapRender render)


restructureCurrentNode render =
    unwrap >> FIZ.restructureCursorWithContext (wrapRender render)


goBackward : OutlineDoc -> Maybe OutlineDoc
goBackward =
    mapMaybe FIZ.goBackward


goForward : OutlineDoc -> Maybe OutlineDoc
goForward =
    mapMaybe FIZ.goForward



-- ForestZipper Extra
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
