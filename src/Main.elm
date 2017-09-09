module Main exposing (..)

import Html exposing (Html, div, h1, img, input, li, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onFocus, onInput)
import Json.Decode as Json


---- MODEL ----


type Direction
    = Left
    | Right


type alias Entry =
    { id : Int
    , description : String
    }


type alias Collection =
    { id : Int
    , description : String
    , entries : List Entry
    , editing : Bool
    }


type alias Model =
    { collections : List Collection, newEntry : String, uid : Int }


seed : List Collection
seed =
    [ { id = 0
      , description = "In Progress"
      , entries =
            [ { id = 0, description = "Hello" }
            , { id = 1, description = "Bye" }
            ]
      , editing = False
      }
    , { id = 1, description = "Done", entries = [ { id = 2, description = "Good morning" }, { id = 3, description = "Good night" } ], editing = False }
    ]


init : ( Model, Cmd Msg )
init =
    ( { collections = seed
      , newEntry = ""
      , uid = 2
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateNewEntry String
    | EditingCollection Collection
    | AddEntry Collection
    | MoveEntry Direction



-- to do a findIndex at, need to compare on some value?


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddEntry collection ->
            case getIndexOf collection model.collections of
                Nothing ->
                    ( { collections = model.collections, newEntry = "", uid = model.uid }, Cmd.none )

                Just index ->
                    ( { collections =
                            List.take index model.collections
                                ++ [ { collection
                                        | entries =
                                            collection.entries
                                                ++ [ { id = model.uid, description = model.newEntry } ]
                                     }
                                   ]
                                ++ List.drop (index + 1) model.collections
                      , newEntry = ""
                      , uid = model.uid + 1
                      }
                    , Cmd.none
                    )

        UpdateNewEntry description ->
            ( { model | newEntry = description }, Cmd.none )

        -- MoveEntry direction ->
        EditingCollection collection ->
            case getIndexOf collection model.collections of
                Nothing ->
                    ( { model | collections = List.take 1 model.collections }, Cmd.none )

                Just index ->
                    if collection.editing == True then
                        ( model, Cmd.none )
                    else
                        ( { model | collections = List.map (\collection -> { collection | editing = False }) (List.take index model.collections) ++ [ { collection | editing = True } ] ++ List.map (\collection_ -> { collection_ | editing = False }) (List.drop (index + 1) model.collections) }, Cmd.none )

        _ ->
            ( { collections = seed, newEntry = "", uid = model.uid }, Cmd.none )



-- update msg model =
--     case msg of
--         UpdateNewTask text ->
--             ( { model | newTask = text }, Cmd.none )
--         AddTask status ->
--             { model
--                 | tasks =
--                     if String.isEmpty model.newTask then
--                         model.tasks
--                     else
--                         model.tasks ++ [ { id = List.length model.tasks, description = model.newTask, status = status } ]
--                 , newTask = ""
--             }
--                 ! []
--         UpdateExistingTask taskId taskName ->
--             let
--                 updateTask task =
--                     if task.id == taskId then
--                         { task | name = taskName }
--                     else
--                         task
--             in
--             { model | tasks = List.map updateTask model.tasks }
--                 ! []
--         -- MoveColumn direction status ->
--         --     if direction == Left then
--         --     -- grab subset of elements up to this in columnOrder
--         _ ->
--             model ! []
---- VIEW ----
-- taskCard : Task -> Html Msg
-- taskCard task =
--     li []
--         [ input [ value task.name, onInput (UpdateExistingTask task.id) ] [ text task.name ] ]
-- filteredTaskCards : String -> List Task -> List (Html Msg)
-- filteredTaskCards status tasks =
--     tasks
--         |> List.filter (\task -> task.status == status)
--         |> List.map taskCard
-- taskCardsList : Model -> Task -> Html Msg
-- taskCardsList model status =
--     div []
--         [ h1 [] [ text status ]
--         , ul [] (filteredTaskCards status model.tasks)
--         , div [] [ addTask model status ]
--         ]
-- onEnter : Msg -> Html.Attribute Msg
-- onEnter msg =
--     let
--         isEnter code =
--             if code == 13 then
--                 Json.succeed msg
--             else
--                 Json.fail "not ENTER"
--     in
--     on "keyup" (Json.andThen isEnter Html.Events.keyCode)
-- addTask : Model -> String -> Html Msg
-- addTask model status =
--     input [ placeholder "Add a task", value model.newTask, onInput UpdateNewTask, onEnter (AddTask status) ]
--         []
-- leftArrow : Model -> String -> Html Msg
-- leftArrow model status =
--     div [ text "<", onClick (MoveColumn Left) ] []


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
            , onEnter (AddEntry collection)
            ]
            []
        ]


viewArrow : Direction -> Html Msg
viewArrow direction =
    case direction of
        Left ->
            span [ onClick (MoveEntry Left) ] [ text "<" ]

        Right ->
            span [ onClick (MoveEntry Right) ] [ text ">" ]


viewEntry : Collection -> Entry -> Html Msg
viewEntry collection entry =
    li []
        [ div []
            [ viewArrow Left
            , input [ value entry.description ] []
            , viewArrow Right
            ]
        ]


viewCollection : Model -> Collection -> Html Msg
viewCollection model collection =
    div []
        [ text collection.description
        , ul [] (List.map (viewEntry collection) collection.entries)
        , viewAddEntry model.newEntry collection
        ]


view : Model -> Html Msg
view model =
    div [ class "flex" ]
        (List.map (viewCollection model) model.collections)



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
