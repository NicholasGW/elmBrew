module FilterDropdown(view, Model, Action, init, update) where

import List exposing (..)
import Signal exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode exposing(..)

--Model

type alias Model = { values : List String
                   , selected : String
                   }

init : List String -> String -> Model
init values selected =
  Model values selected

--Update

type Action = Update String

update : Action -> Model -> Model
update action model =
  case action of
    Update str ->
      { model | selected = str}


--View
view : Signal.Address Action -> Model -> Html
view address model =
   select [ on "change" targetValue (\str -> Signal.message address (Update str))]
          (optionView model.values)

optionView : List String -> List Html
optionView values =
  List.map (\str -> option [ value str
                           , property "label" (Json.Encode.string str)
                           ] []) values
