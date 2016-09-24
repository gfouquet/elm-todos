module Todos exposing (..)

import Html exposing (Html, button, div, text, a, ul, li, input)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Debug exposing (log)


-- MAIN
main = App.beginnerProgram { model = model, view = view, update = update }

-- MODEL
type alias Model = List Todo
model = [
    Todo "Learn Elm" False
    , Todo "Gouge away" True]

type alias Todo = { label: String, done: Bool }

-- UPDATE
type Msg = NoOp | ToggleDone Int | Add String

update : Msg -> Model -> Model
update msg model =
    case log "msg" msg of
        NoOp -> model
        ToggleDone dx -> toggleDone dx model
        Add label -> todo label :: List.reverse model |> List.reverse

toggleDone dx model =
    List.indexedMap
        ( \n todo ->
            if n == dx then { todo | done = not todo.done }
            else todo )
        model

todo label =
    { label = label, done = False }
-- VIEW

view : Model -> Html Msg
view model =
    div [] [
        div []
        [
            input [ placeholder "Todo" ] []
            , button [onClick (Add "Velouria")] [text "Add"]
        ]
        , ul [] (List.indexedMap todoToLiMapper model)
        , div []
        [
            a[style [("font-weight", "bold")]] [text "All"]
            , text " "
            , a[] [text "Pending"]
            , text " "
            , a[] [text "Done"]
        ]
    ]

todoToLiMapper dx todo =
    li [style [( "text-decoration", liDecoration todo )]
        , onClick (ToggleDone dx) ]
        [text todo.label]

liDecoration todo =
    case todo.done of
        True -> "line-through"
        False -> "none"