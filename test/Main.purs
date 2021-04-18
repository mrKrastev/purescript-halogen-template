module Test.Main where

import Prelude

import App.EntryPoint (generateFlavourText)
import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log $ generateFlavourText 60.0 66.0
