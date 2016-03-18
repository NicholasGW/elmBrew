module BeerCard (view) where

import Html exposing (..)


view beer =
  div [] [text (beer.name ++ " : " ++  beer.brewery)]
