module Main exposing (..)

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

1.  Model - a full definition of the application's state
2.  Update - a way to step the application state forward
3.  View - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>

-}

import Animator
import Animator.Css
import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Task
import Time


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Elm • TodoMVC", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = subscriptions
        }


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.Css.watching .entries
            (\newEntries model ->
                { model | entries = newEntries }
            )



-- port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , cmds
      --, Cmd.batch [ setStorage newModel, cmds ]
    )



-- MODEL
-- The full application state of our todo app.


type alias Model =
    { entries : Animator.Timeline (List AnimatedEntry)
    , field : String
    , uid : Int
    , visibility : String
    }


type alias AnimatedEntry =
    { entry : Entry, presence : Presence }


updateAnimatedEntry : (Entry -> Entry) -> AnimatedEntry -> AnimatedEntry
updateAnimatedEntry updateFunction animatedEntry =
    { animatedEntry | entry = updateFunction animatedEntry.entry }


type Presence
    = Added
    | Present
    | Deleted


type alias Entry =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    }


emptyModel : Model
emptyModel =
    { entries = Animator.init []
    , visibility = "All"
    , field = ""
    , uid = 0
    }


newEntry : String -> Int -> Entry
newEntry desc id =
    { description = desc
    , completed = False
    , editing = False
    , id = id
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel
    , Cmd.none
    )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | UpdateField String
    | EditingEntry Int Bool
    | UpdateEntry Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String
    | Tick Time.Posix



-- How we update our Model on a given Msg?


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Add ->
            ( { model
                | uid = model.uid + 1
                , field = ""
                , entries =
                    if String.isEmpty model.field then
                        model.entries

                    else
                        let
                            newEntries =
                                Animator.current model.entries ++ [ { entry = newEntry model.field model.uid, presence = Added } ]
                        in
                        model.entries |> Animator.go Animator.verySlowly newEntries
              }
            , Cmd.none
            )

        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )

        EditingEntry id isEditing ->
            let
                updateEntry entry =
                    if entry.id == id then
                        { entry | editing = isEditing }

                    else
                        entry

                focus =
                    Dom.focus ("todo-" ++ String.fromInt id)
            in
            ( { model | entries = model.entries |> Animator.go Animator.immediately (List.map (updateAnimatedEntry updateEntry) (Animator.current model.entries)) }
            , Task.attempt (\_ -> NoOp) focus
            )

        UpdateEntry id task ->
            let
                updateEntry entry =
                    if entry.id == id then
                        { entry | description = task }

                    else
                        entry
            in
            ( { model | entries = model.entries |> Animator.go Animator.immediately (List.map (updateAnimatedEntry updateEntry) (Animator.current model.entries)) }
            , Cmd.none
            )

        Delete id ->
            ( { model | entries = model.entries |> Animator.go Animator.immediately (List.filter (\t -> t.entry.id /= id) (Animator.current model.entries)) }
            , Cmd.none
            )

        DeleteComplete ->
            ( { model | entries = model.entries |> Animator.go Animator.immediately (List.filter (not << .completed << .entry) (Animator.current model.entries)) }
            , Cmd.none
            )

        Check id isCompleted ->
            let
                updateEntry entry =
                    if entry.id == id then
                        { entry | completed = isCompleted }

                    else
                        entry
            in
            ( { model | entries = model.entries |> Animator.go Animator.immediately (List.map (updateAnimatedEntry updateEntry) (Animator.current model.entries)) }
            , Cmd.none
            )

        CheckAll isCompleted ->
            let
                updateEntry entry =
                    { entry | completed = isCompleted }
            in
            ( { model | entries = model.entries |> Animator.go Animator.immediately (List.map (updateAnimatedEntry updateEntry) (Animator.current model.entries)) }
            , Cmd.none
            )

        ChangeVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )

        Tick newTime ->
            ( Animator.update newTime animator model
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"
        , style "visibility" "hidden"
        ]
        [ section
            [ class "todoapp" ]
            [ lazy viewInput model.field
            , lazy2 viewEntries model.visibility model.entries
            , lazy2 viewControls model.visibility (List.map .entry (Animator.current model.entries))
            ]
        , infoFooter
        ]


viewInput : String -> Html Msg
viewInput task =
    header
        [ class "header" ]
        [ h1 [] [ text "todos" ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value task
            , name "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)



-- VIEW ALL ENTRIES


viewEntries : String -> Animator.Timeline (List AnimatedEntry) -> Html Msg
viewEntries visibility animatedEntries =
    let
        isVisible todo =
            case visibility of
                "Completed" ->
                    todo.entry.completed

                "Active" ->
                    not todo.entry.completed

                _ ->
                    True

        allCompleted =
            List.all (.completed << .entry) (Animator.current animatedEntries)

        cssVisibility =
            if List.isEmpty (Animator.current animatedEntries) then
                "hidden"

            else
                "visible"
    in
    section
        [ class "main"
        , style "visibility" cssVisibility
        ]
        [ input
            [ class "toggle-all"
            , type_ "checkbox"
            , name "toggle"
            , checked allCompleted
            , onClick (CheckAll (not allCompleted))
            ]
            []
        , label
            [ for "toggle-all" ]
            [ text "Mark all as complete" ]
        , Keyed.ul [ class "todo-list" ] <|
            List.map (viewKeyedEntry animatedEntries) (List.filter isVisible (Animator.current animatedEntries))
        ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntry : Animator.Timeline (List AnimatedEntry) -> AnimatedEntry -> ( String, Html Msg )
viewKeyedEntry timeline todo =
    ( String.fromInt todo.entry.id, viewEntry timeline todo )


viewEntry : Animator.Timeline (List AnimatedEntry) -> AnimatedEntry -> Html Msg
viewEntry timeline ({ entry } as animatedTodo) =
    Animator.Css.node "li"
        timeline
        [ Animator.Css.height <|
            \entries ->
                let
                    presence =
                        entries |> List.filter ((.id << .entry) >> (==) entry.id) |> List.head |> Maybe.map .presence
                in
                if presence == Just Added then
                    Animator.at 60

                else
                    Animator.at 0
        ]
        [ classList [ ( "completed", animatedTodo.entry.completed ), ( "editing", animatedTodo.entry.editing ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked animatedTodo.entry.completed
                , onClick (Check animatedTodo.entry.id (not animatedTodo.entry.completed))
                ]
                []
            , label
                [ onDoubleClick (EditingEntry animatedTodo.entry.id True) ]
                [ text animatedTodo.entry.description ]
            , button
                [ class "destroy"
                , onClick (Delete animatedTodo.entry.id)
                ]
                []
            ]
        , input
            [ class "edit"
            , value animatedTodo.entry.description
            , name "title"
            , id ("todo-" ++ String.fromInt animatedTodo.entry.id)
            , onInput (UpdateEntry animatedTodo.entry.id)
            , onBlur (EditingEntry animatedTodo.entry.id False)
            , onEnter (EditingEntry animatedTodo.entry.id False)
            ]
            []
        ]



-- VIEW CONTROLS AND FOOTER


viewControls : String -> List Entry -> Html Msg
viewControls visibility entries =
    let
        entriesCompleted =
            List.length (List.filter .completed entries)

        entriesLeft =
            List.length entries - entriesCompleted
    in
    footer
        [ class "footer"
        , hidden (List.isEmpty entries)
        ]
        [ lazy viewControlsCount entriesLeft
        , lazy viewControlsFilters visibility
        , lazy viewControlsClear entriesCompleted
        ]


viewControlsCount : Int -> Html Msg
viewControlsCount entriesLeft =
    let
        item_ =
            if entriesLeft == 1 then
                " item"

            else
                " items"
    in
    span
        [ class "todo-count" ]
        [ strong [] [ text (String.fromInt entriesLeft) ]
        , text (item_ ++ " left")
        ]


viewControlsFilters : String -> Html Msg
viewControlsFilters visibility =
    ul
        [ class "filters" ]
        [ visibilitySwap "#/" "All" visibility
        , text " "
        , visibilitySwap "#/active" "Active" visibility
        , text " "
        , visibilitySwap "#/completed" "Completed" visibility
        ]


visibilitySwap : String -> String -> String -> Html Msg
visibilitySwap uri visibility actualVisibility =
    li
        [ onClick (ChangeVisibility visibility) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text visibility ]
        ]


viewControlsClear : Int -> Html Msg
viewControlsClear entriesCompleted =
    button
        [ class "clear-completed"
        , hidden (entriesCompleted == 0)
        , onClick DeleteComplete
        ]
        [ text ("Clear completed (" ++ String.fromInt entriesCompleted ++ ")")
        ]


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    animator |> Animator.toSubscription Tick model
