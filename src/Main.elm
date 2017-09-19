port module Main exposing (..)

import Css
import Dom exposing (focus)
import Html exposing (Html, button, div, h1, img, input, li, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onBlur, onClick, onFocus, onInput)
import Json.Decode as Json
import Navigation exposing (Location, newUrl)
import Task
import UrlParser exposing ((</>), int, map, oneOf, parseHash, parsePath, s, string, top)


styles : List Css.Style -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


port setStorage : Model -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )


type Direction
    = Left
    | Right


type Route
    = Board Int
    | Card Int
    | Home
    | NotFound


type alias NewCollection =
    { description : String
    , inFocus : Bool
    }


type alias Entry =
    { id : Int
    , description : String
    , editing : Bool
    }


type alias Collection =
    { id : Int
    , description : String
    , entries : List Entry
    , editing : Bool
    }


type alias Model =
    { collections : List Collection
    , newEntry : String
    , entryUid :
        Int
    , collectionUid : Int
    , newCollection : NewCollection
    , location : Location

    -- , route : Maybe Route
    -- , openEntry : Maybe Int
    }



-- type alias EntryOrCollection
--     = Entry
--     | Collection


seed : List Collection
seed =
    [ { id = 0
      , description = "In Progress"
      , entries =
            [ { id = 0, description = "Hello", editing = False }
            , { id = 1, description = "Bye", editing = False }
            ]
      , editing = False
      }
    , { id = 1
      , description = "Done"
      , entries =
            [ { id = 2
              , description =
                    "Good morning"
              , editing = False
              }
            , { id = 3, description = "Good night", editing = False }
            ]
      , editing = False
      }
    ]


fakeInitialState : Location -> Model
fakeInitialState location =
    { collections = seed
    , newEntry = ""
    , entryUid = 4
    , collectionUid = 2
    , newCollection =
        { inFocus = False
        , description = ""
        }
    , location = { location | username = "placeholder", password = "placeholder" }
    }


init : Maybe Model -> Location -> ( Model, Cmd Msg )
init localStorageState location =
    ( Maybe.withDefault (fakeInitialState location)
        (Maybe.map (\localStorageState_ -> { localStorageState_ | location = location }) localStorageState)
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateNewEntry String
    | IsEditingEntry Collection Entry Bool
    | DeleteEntry Int
    | SetEntryNotEditing Collection Entry
    | OpenEntry Entry
    | UpdateNewCollection String
    | ToggleNewCollectionFocus Bool
    | AddNewCollection
    | EditingCollection Collection
    | AddEntry Int
    | MoveEntryBetweenCollections Entry Collection Direction
    | MoveEntryWithinCollection Collection
    | UrlChange Location
    | GoHome


getIndexOf : a -> List a -> Maybe Int
getIndexOf element list =
    let
        indexOf list_ index =
            case list_ of
                [] ->
                    Nothing

                a :: rest ->
                    if a == element then
                        Just index
                    else
                        indexOf rest (index + 1)
    in
    indexOf list 0


getElementAt : Int -> List a -> Maybe a
getElementAt index list =
    if index < 0 then
        Nothing
    else
        List.head (List.drop index list)


removeEntryFromCollection : Int -> Collection -> Collection
removeEntryFromCollection entryId collection =
    { collection | entries = List.filter (\entry -> entry.id /= entryId) collection.entries }


addEntryToEndOfCollection : Entry -> Collection -> Collection
addEntryToEndOfCollection entry collection =
    { collection | entries = collection.entries ++ [ entry ] }


getNextElement : Maybe Int -> List a -> Maybe a
getNextElement index list =
    case index of
        Nothing ->
            Nothing

        Just index_ ->
            getElementAt (index_ + 1) list


getPreviousElement : Maybe Int -> List a -> Maybe a
getPreviousElement index list =
    case index of
        Nothing ->
            Nothing

        Just index_ ->
            getElementAt (index_ - 1) list


getPreviousCollection : Maybe Int -> List Collection -> Maybe Collection
getPreviousCollection index collections =
    getPreviousElement index collections


getNextCollection : Maybe Int -> List Collection -> Maybe Collection
getNextCollection index collections =
    getNextElement index collections


setDescription : String -> NewCollection -> NewCollection
setDescription description newCollection =
    { newCollection | description = description }


focusNewCollectionInput : Result Dom.Error a -> Msg
focusNewCollectionInput result =
    case result of
        Ok _ ->
            ToggleNewCollectionFocus True

        Err _ ->
            ToggleNewCollectionFocus False


focusEntry : Result Dom.Error a -> Msg
focusEntry result =
    case result of
        _ ->
            NoOp


getCardAndCollection : Int -> List Collection -> Maybe ( Entry, Collection )
getCardAndCollection cardId collections =
    case collections of
        [] ->
            Nothing

        collection :: rest ->
            let
                card =
                    collection.entries
                        |> List.filter (\entry -> entry.id == cardId)
                        |> List.head
            in
            Maybe.withDefault (getCardAndCollection cardId rest) (Maybe.map (\card_ -> Just ( card_, collection )) card)


updateCollectionInModel : Collection -> Model -> Model
updateCollectionInModel updatedCollection model =
    { model
        | collections =
            List.map
                (\collection_ ->
                    if collection_.id == updatedCollection.id then
                        updatedCollection
                    else
                        collection_
                )
                model.collections
    }


getDOMidEntry : Entry -> String
getDOMidEntry { id, description } =
    String.join "-" [ toString id, description ]


getDOMidCollection : Entry -> String
getDOMidCollection { id, description } =
    String.join "-" [ toString id, description ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddEntry collectionId ->
            let
                getCollection collection =
                    if collectionId == collection.id then
                        addEntryToEndOfCollection
                            { id = model.entryUid
                            , description = model.newEntry
                            , editing = False
                            }
                            collection
                    else
                        collection
            in
            ( { model
                | collections = List.map getCollection model.collections
                , entryUid = model.entryUid + 1
                , newEntry = ""
              }
            , Cmd.none
            )

        UpdateNewEntry description ->
            ( { model | newEntry = description }, Cmd.none )

        IsEditingEntry collection entry isEditing ->
            let
                editingEntry =
                    { entry | editing = True }

                updatedCollection =
                    { collection
                        | entries =
                            List.map
                                (\entry_ ->
                                    if entry_.id == editingEntry.id then
                                        editingEntry
                                    else
                                        entry_
                                )
                                collection.entries
                    }
            in
            ( updateCollectionInModel updatedCollection model
            , Task.attempt focusEntry (focus (getDOMidEntry entry))
            )

        SetEntryNotEditing collection entry ->
            let
                updatedEntry =
                    { entry | editing = False }

                updatedCollection =
                    { collection
                        | entries =
                            List.map
                                (\entry_ ->
                                    if entry_.id == updatedEntry.id then
                                        updatedEntry
                                    else
                                        entry_
                                )
                                collection.entries
                    }
            in
            ( updateCollectionInModel updatedCollection model
            , Cmd.none
            )

        OpenEntry entry ->
            ( model, Navigation.newUrl ("/entries/" ++ toString entry.id) )

        DeleteEntry entryId ->
            case getCardAndCollection entryId model.collections of
                Just ( card, collection ) ->
                    let
                        updatedCollection =
                            removeEntryFromCollection entryId collection

                        updatedModel =
                            updateCollectionInModel updatedCollection model
                    in
                    ( updatedModel, Navigation.newUrl "/" )

                Nothing ->
                    ( model, Cmd.none )

        ToggleNewCollectionFocus bool ->
            let
                newCollection_ =
                    model.newCollection

                newCollection =
                    { newCollection_ | inFocus = bool }
            in
            ( { model | newCollection = newCollection }, Cmd.none )

        UpdateNewCollection description ->
            let
                newCollection =
                    model.newCollection
                        |> setDescription description
            in
            ( { model | newCollection = newCollection }, Cmd.none )

        AddNewCollection ->
            let
                collectionToAdd =
                    { id = model.collectionUid
                    , description =
                        model.newCollection.description
                    , entries = []
                    , editing = False
                    }

                newCollection =
                    { description = "", inFocus = False }
            in
            ( { model
                | collections = model.collections ++ [ collectionToAdd ]
                , newCollection = newCollection
                , collectionUid = model.collectionUid + 1
              }
            , Task.attempt focusNewCollectionInput (focus "add-new-collection-input")
            )

        MoveEntryBetweenCollections entry collection direction ->
            let
                originalCollectionIndex =
                    getIndexOf collection model.collections

                destinationCollectionWithEntry =
                    case direction of
                        Left ->
                            let
                                previousCollection =
                                    getPreviousCollection originalCollectionIndex model.collections
                            in
                            previousCollection
                                |> Maybe.map (addEntryToEndOfCollection entry)

                        Right ->
                            let
                                nextCollection =
                                    getNextCollection originalCollectionIndex model.collections
                            in
                            nextCollection
                                |> Maybe.map (addEntryToEndOfCollection entry)

                originalCollectionWithoutEntry =
                    { collection | entries = List.filter (\entry_ -> entry_.id /= entry.id) collection.entries }
            in
            Maybe.withDefault ( model, Cmd.none )
                (Maybe.map
                    (\validDestinationCollection ->
                        ( { model
                            | collections =
                                List.map
                                    (\collection_ ->
                                        if collection_.id == validDestinationCollection.id then
                                            validDestinationCollection
                                        else if collection_.id == originalCollectionWithoutEntry.id then
                                            originalCollectionWithoutEntry
                                        else
                                            collection_
                                    )
                                    model.collections
                          }
                        , Cmd.none
                        )
                    )
                    destinationCollectionWithEntry
                )

        EditingCollection collection ->
            case getIndexOf collection model.collections of
                Nothing ->
                    ( model, Cmd.none )

                Just index ->
                    if collection.editing == True then
                        ( model, Cmd.none )
                    else
                        let
                            collections_ =
                                List.map
                                    (\collection_ ->
                                        if collection_.id == collection.id then
                                            { collection_ | editing = True }
                                        else
                                            { collection_ | editing = False }
                                    )
                                    model.collections
                        in
                        ( { model | collections = collections_, newEntry = "" }, Cmd.none )

        UrlChange location ->
            let
                locationWithUsernamePassword =
                    { location | username = "placeholder", password = "placeholder" }
            in
            ( { model | location = locationWithUsernamePassword }, Cmd.none )

        -- so navigating to "" does not work, but putting a "/" resets the url to have an empty path?
        GoHome ->
            ( model, Navigation.newUrl "/" )

        _ ->
            ( model, Cmd.none )



-- JS doesn't have the concept of Union Type, so we cannot use Route in the model. Makes more sense to store the
-- current location; in the view, we can translate the location into a type of Route and display that... I think


locationToRoute : Location -> Maybe Route
locationToRoute location =
    let
        route =
            oneOf
                [ UrlParser.map Card (s "entries" </> int)
                , UrlParser.map Home top
                ]

        _ =
            Debug.log "Urlchange" route
    in
    parsePath route location



-- keyboard event key decoding... gotta understand this a bit better


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not Enter"
    in
    on "keyup" (Json.andThen isEnter Html.Events.keyCode)


viewAddEntry : String -> Collection -> Html Msg
viewAddEntry newEntry collection =
    div []
        [ input
            [ placeholder "Add something for safekeeping"
            , value
                (if collection.editing == True then
                    newEntry
                 else
                    ""
                )
            , onInput UpdateNewEntry
            , onFocus (EditingCollection collection)
            , onEnter (AddEntry collection.id)
            ]
            []
        ]


isFirstCollection : Collection -> List Collection -> Maybe Bool
isFirstCollection collection collections =
    let
        index =
            getIndexOf collection collections
    in
    Maybe.map (\index_ -> index_ == 0) index


isLastCollection : Collection -> List Collection -> Maybe Bool
isLastCollection collection collections =
    let
        index =
            getIndexOf collection collections
    in
    Maybe.map (\index_ -> index_ == (List.length collections - 1)) index


viewArrow : Entry -> Collection -> Direction -> Html Msg
viewArrow entry collection direction =
    case direction of
        Left ->
            span [ onClick (MoveEntryBetweenCollections entry collection Left) ] [ text "<" ]

        Right ->
            span [ onClick (MoveEntryBetweenCollections entry collection Right) ] [ text ">" ]


viewEntryEditing : Collection -> Entry -> Html Msg
viewEntryEditing collection entry =
    if entry.editing == True then
        input [ value entry.description, onBlur (SetEntryNotEditing collection entry), id (getDOMidEntry entry) ] []
    else
        span [ onClick (OpenEntry entry), id (getDOMidEntry entry) ]
            [ text entry.description ]


viewEntry : List Collection -> Collection -> Entry -> Html Msg
viewEntry collections collection entry =
    li []
        [ div [ class "flex flex-justify-between pad border" ]
            [ if isFirstCollection collection collections /= Just True then
                viewArrow entry collection Left
              else
                text ""
            , viewEntryEditing collection entry
            , if entry.editing == False then
                span [ onClick (IsEditingEntry collection entry True) ] [ text "E" ]
              else
                text ""
            , if isLastCollection collection collections /= Just True then
                viewArrow entry collection Right
              else
                text ""
            ]
        ]


viewCollection : Model -> Collection -> Html Msg
viewCollection model collection =
    div []
        [ text collection.description
        , ul [] (List.map (viewEntry model.collections collection) collection.entries)
        , viewAddEntry model.newEntry collection
        ]


viewAddCollectionActions : Html Msg
viewAddCollectionActions =
    div [ class "flex flex-justify-start" ]
        [ button [ name "Save", onClick AddNewCollection ] [ text "Save" ]
        , span [] [ text "X" ]
        ]


viewAddCollection : Bool -> Html Msg
viewAddCollection inFocus =
    div [ class "flex flex-direction-column" ]
        [ input
            [ id "add-new-collection-input"
            , placeholder "Add a list..."
            , onInput UpdateNewCollection
            , onFocus (ToggleNewCollectionFocus True)
            ]
            []
        , if inFocus == True then
            viewAddCollectionActions
          else
            text ""
        ]


viewEntryDetailed : Maybe ( Entry, Collection ) -> Html Msg
viewEntryDetailed cardCollectionTuple =
    Maybe.withDefault (text "")
        (Maybe.map
            (\( card, collection ) ->
                div [ class "modal-content flex flex-direction-column" ]
                    [ span [ class "text-align-right", onClick GoHome ] [ text "X" ]
                    , h1 [] [ text card.description ]
                    , button [ styles [ Css.backgroundColor (Css.hex "FF0000") ], onClick (DeleteEntry card.id) ] [ text "Delete" ]
                    ]
            )
            cardCollectionTuple
        )



-- case card of
--     Just foundCard ->
--         div [ class "modal-content flex flex-direction-column" ]
--             [ span [ class "text-align-right", onClick GoHome ] [ text "X" ]
--             , h1 [] [ text foundCard.description ]
--             , button [ styles [ Css.backgroundColor (Css.hex "FF0000") ], onClick (DeleteEntry foundCard.id) ] [ text "Delete" ]
--             ]
--     Nothing ->
--         text ""


viewModal : Int -> List Collection -> Html Msg
viewModal cardId collections =
    div [ class "modal flex flex-justify-center flex-align-center" ]
        [ viewEntryDetailed (getCardAndCollection cardId collections) ]


view : Model -> Html Msg
view model =
    case locationToRoute model.location of
        Just (Board id) ->
            div [ class "flex" ]
                (List.map (viewCollection model) model.collections
                    ++ [ viewAddCollection model.newCollection.inFocus ]
                )

        Just NotFound ->
            div [ class "flex" ]
                (List.map (viewCollection model) model.collections
                    ++ [ viewAddCollection model.newCollection.inFocus ]
                )

        Just (Card cardId) ->
            div [ class "flex" ]
                (List.map (viewCollection model) model.collections
                    ++ [ viewAddCollection model.newCollection.inFocus ]
                    ++ [ viewModal cardId model.collections ]
                )

        Just Home ->
            div [ class "flex" ]
                (List.map (viewCollection model) model.collections
                    ++ [ viewAddCollection model.newCollection.inFocus ]
                )

        Nothing ->
            div [ class "flex" ]
                (List.map (viewCollection model) model.collections
                    ++ [ viewAddCollection model.newCollection.inFocus ]
                )


main : Program (Maybe Model) Model Msg
main =
    Navigation.programWithFlags UrlChange
        { view = view
        , init = init
        , update = updateWithStorage
        , subscriptions = always Sub.none
        }
