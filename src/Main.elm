import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import StartApp.Simple as StartApp

import BeerCard exposing (..)



--Model
type alias Beer = { name: String, brewery: String}
type alias Model = { beers: List Beer, search : String }

model = { beers = [ { name = "Tom Waits For No One"
                    , brewery = "Good Robot Brewing Co"
                    }
                  ]
        , search = ""
      }


--Update

type Action = Search String

update action model =
  case action of
    Search text ->
      { model | search = text }


--View

searchBar string address =
  input [ placeholder "Search"
        , value string
        , on "input" targetValue (\str -> Signal.message address (Search str))
        ] []

beerCards beers =
  List.map BeerCard.view beers

view: Signal.Address Action -> Model -> Html
view address model =
  div [] (searchBar model.search address :: beerCards model.beers)

main =
  StartApp.start({model = model, view = view, update = update})
