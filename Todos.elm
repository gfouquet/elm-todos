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
        , ul [] (model.todos |> filterTodos model.filter |> List.indexedMap todoToLiMapper)
        , div []
            [ a
                [ style [("font-weight", filterWeight FilterAll model.filter)]
                , onClick (ApplyFilter FilterAll)
                ] [text "All"]
            , text " "
            , a
                [ style [("font-weight", filterWeight FilterPending model.filter)]
                , onClick (ApplyFilter FilterPending)
                ] [text "Pending"]
            , text " "
            , a
                [ style [("font-weight", filterWeight FilterDone model.filter)]
                , onClick (ApplyFilter FilterDone)
                ] [text "Done"]
            ]
        ]

todoToLiMapper : Int -> Todo -> Html Msg
todoToLiMapper dx todo =
    li
        [style [( "text-decoration", liDecoration todo )]
        , onClick (ToggleDone dx)
        ]
        [text todo.label]

liDecoration : Todo -> String
liDecoration todo =
    case todo.done of
        True -> "line-through"
        False -> "none"

filterTodos : Filter -> List Todo -> List Todo
filterTodos filter todos =
    case filter of
    FilterAll -> todos
    FilterPending -> List.filter (\todo -> not todo.done) todos
    FilterDone -> List.filter (\todo -> todo.done) todos

filterWeight : Filter -> Filter -> String
filterWeight current active =
    if current == active then "bold" else "normal"
