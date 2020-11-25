module App.MainApp where

import Prelude

import CSS (StyleM, color)
import CSS.Color (red, green)
import Control.Apply (lift2)
import Data.Array ((!!))

import Data.DateTime (Time)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split)
import Data.Time (diff)
import Data.Time.Duration (Seconds)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Now (nowTime)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP









type State = {  
              input::String,
              wrongWordCounter::Int,
              wordCounter::Int,
              myText::String,
              myTimeNow:: Maybe Time,
              myPreviousTime:: Maybe Time,
              timeDifference::Maybe Seconds}

data Action = Update | SendInput String


fromJustString :: Maybe String -> String
fromJustString Nothing = ""
fromJustString (Just s) = s


component :: forall q i o m.MonadEffect m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> { wordCounter: 0, input:"", wrongWordCounter: 0, myText:fromJustString (myWords !! 0), myTimeNow: Nothing, myPreviousTime: Nothing, timeDifference:Nothing}
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  
  HH.div 
      [HCSS.style do 
                wrongWordIndicator state
              ] 
    [HH.p_
        [ HH.text $ show (fromJustString (myWords !! state.wordCounter)) <> " Most Recent Time: " <> show state.myTimeNow <>" Last Recorded Time: "<> show state.myPreviousTime <> " My time difference: " <> show state.timeDifference]
      
      ,HH.input
        [ HP.id_ "inp",
        HE.onValueChange \s -> Just (SendInput s)
         ]
    ,HH.p_
        [ HH.text $  " " <> show state.input <> " "<> show (state.wrongWordCounter) <> " wrong words" <> show myWords ]
    , HH.button
        [ HE.onClick \_ -> Just Update ]
        [ HH.text "Update" ]
    ]
    



wrongWordIndicator :: forall t3 t8.
  Eq t3 => { input :: t3
           , myText :: t3
           | t8
           }
           -> StyleM Unit
wrongWordIndicator state
    | state.input == state.myText = do 
                          color green
    | otherwise = color red
    

incrementor :: forall t8. Eq t8 => t8 -> t8 -> Int
incrementor input word
    | input == word = 0
    | otherwise = 1



handleAction :: forall cs o m.MonadEffect m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Update ->
    do
     log(myParagraph)
     log(myarrayedstring)
  SendInput s ->
    do
      
      mynowtime <- liftEffect nowTime
      H.modify_ \st -> st { wordCounter= st.wordCounter + 1,input = s, myText=fromJustString (myWords !!st.wordCounter)}
      H.modify_ \st -> st {myPreviousTime=st.myTimeNow, myTimeNow = Just mynowtime, timeDifference = lift2 diff (Just mynowtime) st.myTimeNow }
      H.modify_ \st -> st { wrongWordCounter=st.wrongWordCounter+incrementor st.input st.myText}






myParagraph :: String
myParagraph = "The world of Dark Souls is a world of cycles. Kingdoms rise and fall, ages come and go, and even time can end and restart as the flame fades and is renewed. These cycles are linked to the First Flame, a mysterious manifestation of life that divides and defines separate states such as heat and cold, or life and death. As the First Flame fades, these differences also begin to fade, such as life and death having little distinction, and humans becoming Undead. The onset of an Age of Dark, the time when the First Flame has fully died, is marked by endless nights, rampant undeath, time, space, and reality breaking down, lands collapsing and converging on one another, people mutating into monsters, darkness covering the world, and the Gods losing their power. To avoid this and prolong the Age of Fire, the bearer of a powerful soul must 'link' themselves to the First Flame, becoming the fuel for another age. If this is not done, the First Flame will eventually die, and an Age of Dark will begin."

myWords :: Array String
myWords = split (Pattern " ") myParagraph

myarrayedstring :: String
myarrayedstring = joinWith "," myWords


  
