module SearchBar (Model, Action, update, view, init) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

--Model

type alias Model = String

init = ""

--Update

type Action = Update String

update : Action -> Model -> Model
update action model =
  case action of
    Update str ->
      str

view : Signal.Address Action -> Model -> Html
view address model =
  input [ placeholder "Search"
        , value model
        , on "input" targetValue (\str -> Signal.message address (Update str))
        ] []
