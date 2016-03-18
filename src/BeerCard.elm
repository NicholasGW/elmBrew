module BeerCard (view) where

import Html exposing (..)
import Maybe exposing (..)

view beer =
  div [] [text (beer.name ++ " : " ++  beer.brewery ++ ( toString (Maybe.withDefault 0 beer.ibu)) )]
