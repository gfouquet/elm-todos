module Todos exposing (..)

import Html exposing(Html, button, div, text, a, ul, li, input)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- MAIN
main : Program Never
main = App.beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Model = String
model = "Learn Elm"

-- UPDATE

type Msg = NoOp

update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp -> model

-- VIEW

view : Model -> Html Msg
view model =
    div [] [
        div []
        [
            input [ placeholder "Todo" ] []
            , button [] [text "Add"]
        ]
        , ul [] [
            li [] [text model]
        ]
        , div []
        [
            a[] [text "All"]
            , text " "
            , a[] [text "Pending"]
            , text " "
            , a[] [text "Done"]

        ]
    ]
