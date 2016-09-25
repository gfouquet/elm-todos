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
        [ Todo "Learn Elm" False
        , Todo "Gouge away" True
        ]
    , input = ""
    , filter = FilterAll
    }

type alias Todo = { label: String, done: Bool }

type Filter = FilterAll | FilterPending | FilterDone

-- UPDATE
type Msg = NoOp | ToggleDone Int | AddTodo String | InputTodo String

update : Msg -> Model -> Model
update msg model =
    case log "msg" msg of
        NoOp -> model
        ToggleDone dx -> { model | todos = toggleDone dx model.todos }
        AddTodo label ->
            { model
            | todos = todo label :: List.reverse model.todos |> List.reverse
            , input = ""
            }
        InputTodo label -> { model | input = label }

toggleDone dx todos =
    List.indexedMap
        ( \n todo ->
            if n == dx then { todo | done = not todo.done }
            else todo
        )
        todos

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
                , disabled (log "input length" String.length model.input == 0)
                ]
                [text "Add"]
            ]
        , ul [] (List.indexedMap todoToLiMapper model.todos)
        , div []
            [ a [style [("font-weight", "bold")]] [text "All"]
            , text " "
            , a[] [text "Pending"]
            , text " "
            , a[] [text "Done"]
            ]
        ]

todoToLiMapper dx todo =
    li
        [style [( "text-decoration", liDecoration todo )]
        , onClick (ToggleDone dx)
        ]
        [text todo.label]

liDecoration todo =
    case todo.done of
        True -> "line-through"
        False -> "none"