


module SupJS where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)

foreign import cleanInputBox :: Unit-> Effect Int
foreign import disableInputBox :: Unit-> Effect Int
foreign import changeParticleSpeed :: (Maybe Number)-> Effect Int