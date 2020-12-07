module Main where

import Prelude

import App.MainApp as MainApp
import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.Element (setAttribute)
import Web.HTML.HTMLElement (toElement)


main :: Effect Unit
main = HA.runHalogenAff do
  -- HA.awaitLoad
  -- traverse_ (runUI MainApp.component unit) =<< HA.selectElement (QuerySelector "body")
  body <- HA.awaitBody
  liftEffect $ setAttribute "style" "background-color:	#23272a;display:flex;flex-wrap:wrap;justify-content:center;color:#FF8C00" (toElement body)
  runUI MainApp.component unit body
