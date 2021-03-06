


module SupJS where

import Prelude

import Effect (Effect)

foreign import cleanInputBox :: Unit-> Effect Int
foreign import disableInputBox :: Unit-> Effect Int
foreign import startGame :: Unit-> Effect Int
foreign import changeParticleSpeed :: Number -> Effect Int
foreign import changeParticleSpeedandWidth :: Number-> Number -> Effect Int
foreign import resizeMagic :: Number -> Effect Int
foreign import renderMagicJs :: Unit -> Effect Number
foreign import fixPVPmagicPositioning :: Unit -> Effect Number
foreign import renderMagicJsPVP :: Unit -> Effect Number
foreign import fixPVPmagic :: Unit -> Effect Number
foreign import changeParticleSpeed2 :: Number -> Effect Int

