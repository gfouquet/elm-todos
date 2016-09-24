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
    { label = "Learn Elm", done = False}
    , { label = "Gouge away", done = True}]

type alias Todo = { label: String, done: Bool }

-- UPDATE
type Msg = NoOp | ToggleDone Int

update : Msg -> Model -> Model
update msg model =
    case log "msg" msg of
        NoOp -> model
        ToggleDone dx -> toggleDone dx model

toggleDone dx model =
    List.indexedMap
        ( \n todo ->
            if n == dx then { todo | done = not todo.done }
            else todo )
        model

-- VIEW

view : Model -> Html Msg
view model =
    div [] [
        div []
        [
            input [ placeholder "Todo" ] []
            , button [] [text "Add"]
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