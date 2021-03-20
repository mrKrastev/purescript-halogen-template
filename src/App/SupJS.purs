


module SupJS where

import Prelude

import Effect (Effect)

foreign import cleanInputBox :: Unit-> Effect Int
foreign import disableInputBox :: Unit-> Effect Int
foreign import changeParticleSpeed :: Number -> Effect Int
foreign import resizeMagic :: Number -> Effect Int
foreign import renderMagicJs :: Number -> Effect String