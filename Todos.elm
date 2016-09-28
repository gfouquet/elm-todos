module Todos exposing (..)

import Html exposing (Html, button, div, text, a, ul, li, input)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Debug exposing (log)

import String exposing (length)

-- MAIN
main = App.beginnerProgram { model = model, view = view, update = update }

-- MODEL
type alias Model = { todos : List Todo, input : String, filter : Filter }
model =
    { todos =
        [ Todo "Velouria" False
        , Todo "Gouge away" True
        , Todo "Subbacultcha" True
        ]
    , input = ""
    , filter = FilterAll
    }

type alias Todo = { label: String, done: Bool }

type Filter = FilterAll | FilterPending | FilterDone

-- UPDATE
type Msg = ToggleDone Int | AddTodo String | InputTodo String | ApplyFilter Filter

update : Msg -> Model -> Model
update msg model =
    case log "msg" msg of
        ToggleDone dx -> { model | todos = toggleDone dx model.todos }
        AddTodo label ->
            { model
            | todos = todo label :: List.reverse model.todos |> List.reverse
            , input = ""
            }
        InputTodo label -> { model | input = label }
        ApplyFilter filter -> { model | filter = filter }

toggleDone dx todos =
    let
        toggler n todo =
            if n == dx
            then { todo | done = not todo.done }
            else todo
    in List.indexedMap toggler todos

todo label =
  { label = label, done = False }

-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input
                [ placeholder "Todo"
                , onInput InputTodo
                , value model.input
                ] []
            , button
                [ onClick (AddTodo model.input)
                , disabled (String.length model.input == 0)
                ]
                [text "Add"]
            ]
        , ul [] (filteredTodos model |> List.indexedMap todoItem)
        , div []
            [ filterButton model FilterAll "All"
            , filterButton model FilterPending "Pending"
            , filterButton model FilterDone "Done"
            ]
        ]

todoItem : Int -> Todo -> Html Msg
todoItem dx todo =
    let
        textDeco : Todo -> String
        textDeco todo =
            case todo.done of
                True -> "line-through"
                False -> "none"
    in
        li
            [style [( "text-decoration", textDeco todo )]
            , onClick (ToggleDone dx)
            ]
            [text todo.label]

filteredTodos : Model -> List Todo
filteredTodos model =
    let {filter, todos} = model
    in
        case filter of
            FilterAll -> todos
            FilterPending -> List.filter (\todo -> not todo.done) todos
            FilterDone -> List.filter (\todo -> todo.done) todos

filterButton : Model -> Filter -> String -> Html Msg
filterButton model event label =
    let
        fontWeight : Filter -> Filter -> String
        fontWeight current active =
            if current == active then "bold" else "normal"
    in
        a
            [style
                [("font-weight", fontWeight model.filter event)
                , ("margin-right", "4px")
                , ("margin-left", "4px")
                ]
            , onClick (ApplyFilter event)
            ]
            [text label]
