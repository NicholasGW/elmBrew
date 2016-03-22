import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (..)
import Json.Encode exposing (..)
import StartApp.Simple as StartApp
import Debug exposing (..)
import FilterDropdown exposing (..)
import SearchBar exposing (..)
import ElmTextSearch exposing (..)

import BeerCard exposing (..)



--Model
type alias Beer = { id : String
                  , name : String
                  , brewery : String
                  , ibu : Maybe Int
                  , abv : Maybe Float
                  , srm : Maybe Int
                  }

type alias Filter = Dict String FilterDropdown.Model

type alias HasId a = { a | id: String}

type alias Model = { search : SearchBar.Model
                   , index: ElmTextSearch.Index Beer
                   , searchResults: List Beer
                   , errorMessage : String
                   , filter: Filter
                   }

createIndex : ElmTextSearch.Index Beer
createIndex =
  ElmTextSearch.new
    { ref = .id
    , fields =
      [ (.name, 5.0)
      , (.brewery, 4.0)
      ]
    }

beers : List Beer
beers = [ { id = "2"
          , name = "Tom Waits"
          , brewery = "Good Robot Brewing Co"
          , ibu = Just 55
          , abv = Nothing
          , srm = Nothing
          }
        ,
          { id = "4"
          , name = "Tom Waits For No One"
          , brewery = "Good Robot Brewing Co"
          , ibu = Nothing
          , abv = Nothing
          , srm = Nothing
          }
        ,
         { id = "3"
         , name = "My sweet beer"
         , brewery = "Sleepy K"
         , ibu = Nothing
         , abv = Nothing
         , srm = Nothing
         }
        ]

breweries = ["Any", "Good Robot Brewing Co"]

beersDict : Dict String Beer
beersDict =
  List.foldr
    (\({id} as doc) dict -> Dict.insert id doc dict)
    Dict.empty
    beers

initialIndex : (ElmTextSearch.Index Beer, List (Int, String))
initialIndex =
  ElmTextSearch.addDocs
    beers
    createIndex


model = { search = SearchBar.init
        , index = (fst initialIndex)
        , searchResults = beers
        , errorMessage = ""
        , filter = Dict.insert "brewery" (FilterDropdown.init breweries "Any") Dict.empty
        }

--Update

type Action = Search SearchBar.Action| UpdateFilter String FilterDropdown.Action

update : Action -> Model -> Model
update action model =
  case action of
    Search act ->
        let
          newSearchText = (SearchBar.update act) model.search
        in
          if newSearchText == ""
          then { model | search = newSearchText
                       , searchResults = beers
                       , errorMessage = ""}
          else
            let
              searchResult = ElmTextSearch.search newSearchText model.index
            in
              case searchResult of
                Err error ->
                  { model | search = newSearchText
                          , searchResults = []
                          , errorMessage = error }
                Ok results ->
                  let
                    newIndex = results |> fst
                    newResults = getDocumentsFromResults beersDict (snd results)
                  in
                    { model | search = newSearchText
                            , searchResults = newResults
                            , index = newIndex
                            , errorMessage = "" }

    UpdateFilter field act ->
      let
        currentFilter = Dict.get field model.filter
      in
        case currentFilter of
          Nothing ->
            model
          Just currentFilter ->
            let
              updatedFilter = FilterDropdown.update act currentFilter
            in
              { model | filter = Dict.insert field updatedFilter model.filter }


idsMatch : String -> HasId a -> Bool
idsMatch id document =
  document.id == id

getDocumentsFromResults : Dict String Beer -> List (String, Float) -> List Beer
getDocumentsFromResults documents results =
  List.filterMap (\result -> Dict.get (fst result) documents) results


--View


filterAsList : Filter -> List (String, FilterDropdown.Model)
filterAsList filter = Dict.toList filter

filterTupleToDropdown : Signal.Address Action -> (String, FilterDropdown.Model) -> Html
filterTupleToDropdown address (field, filterModel) =
  FilterDropdown.view (Signal.forwardTo address (UpdateFilter field)) filterModel

view: Signal.Address Action -> Model -> Html
view address model =
    let
      --filteredBeers = List.filter (filterByPredicate model.filterPredicate) model.searchResults
      filterList = filterAsList model.filter
      dropdowns = List.map (filterTupleToDropdown address) filterList
    in
      div [] (  SearchBar.view (Signal.forwardTo address Search) model.search
             :: div [] [(text model.errorMessage)]
             :: dropdowns
             --:: List.map BeerCard.view filteredBeers
             )

main =
  StartApp.start({model = model, view = view, update = update})
