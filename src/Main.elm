import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import StartApp.Simple as StartApp

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
type alias HasId a = { a | id: String}

type alias Model = { searchText : String
                   , index: ElmTextSearch.Index Beer
                   , searchResults: List Beer
                   , errorMessage : String
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
        ]

initialIndex : (ElmTextSearch.Index Beer, List (Int, String))
initialIndex =
  ElmTextSearch.addDocs
    beers
    createIndex


model = { searchText = ""
        , index = (fst initialIndex)
        , searchResults = beers
        , errorMessage = ""
        }



--Update

type Action = Search String

update action model =
  case action of
    Search text ->
      if text == ""
        then { model | searchText = text, searchResults = beers, errorMessage = ""}
        else
          let
            searchResult = ElmTextSearch.search text model.index
          in
            case searchResult of
              Err error ->
                { model | searchText = text, searchResults = [], errorMessage = error }
              Ok results ->
                let
                  newIndex = results |> fst
                  newResults = getDocumentsFromResults beers (snd results)
                in
                  { model | searchText = text
                          , searchResults = newResults
                          , index = newIndex
                          , errorMessage = "" }


getSingleDocumentFromList : String -> List (HasId a) -> List (HasId a)
getSingleDocumentFromList id documents =
  List.filter (idsMatch id) documents

idsMatch : String -> HasId a -> Bool
idsMatch id document =
  document.id == id

getDocumentsFromResults : List (HasId a) -> List (String, Float) -> List (HasId a)
getDocumentsFromResults documents results =
  List.concat (List.map (\result -> getSingleDocumentFromList (fst result) documents ) results)


--View

searchBar : String -> Signal.Address Action -> Html
searchBar string address =
  input [ placeholder "Search"
        , value string
        , on "input" targetValue (\str -> Signal.message address (Search str))
        ] []


view: Signal.Address Action -> Model -> Html
view address model =
    div [] (  searchBar model.searchText address
           :: div [] [(text model.errorMessage)]
           :: List.map BeerCard.view model.searchResults
           )

main =
  StartApp.start({model = model, view = view, update = update})
