module App.MainApp where

import Prelude

import CSS (Float, StyleM, color, white)
import CSS.Color (red, green)
import Control.Apply (lift2)
import Data.Array ((!!))
import Data.Array.NonEmpty (elemLastIndex)
import Data.DateTime (Time)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split)
import Data.Time (diff)
import Data.Time.Duration (Seconds(..))
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Now (nowTime)
import Halogen (SubscriptionId, liftEffect, unsubscribe)
import Halogen as H
import Halogen.HTML (style_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (style)
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (Emitter)
import Halogen.Query.EventSource as ES
import SupJS (cleanInputBox, disableInputBox)









type State = {  
              input::String,
              wrongWordCounter::Int,
              wordCounter::Int,
              myText::String,
              myTimeNow:: Maybe Time,
              myFirstTime:: Maybe Time,
              timeDifference::Maybe Seconds,
              totalTypingTime::Maybe Seconds,
              timer::Int,
              timerIsRunning::Boolean,
              wpm::Maybe Number}

data Action = Update | SendInput String | Decrement SubscriptionId


fromJustString :: Maybe String -> String
fromJustString Nothing = ""
fromJustString (Just s) = s

calcWPM:: Int-> Maybe Seconds -> Maybe Number
calcWPM wordsCount (Just(Seconds sec)) = Just( ((toNumber wordsCount) / sec) * toNumber 60 )
calcWPM wordsCount Nothing = Nothing

component :: forall q i o m. MonadEffect m => MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> {wpm:Nothing,
    timerIsRunning:false,
    timer:10, wordCounter: 0,
     input:"", 
     wrongWordCounter: 0,
      myText:fromJustString (myWords !! 0),
       myTimeNow: Nothing, myFirstTime: Nothing, timeDifference:Nothing,
       totalTypingTime:Nothing}
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
 HH.div
 [style "width:50%; background-color:#2c2f33;"]
 [
    HH.div  
    [style"display:flex; flex-wrap:wrap;justify-content:center;justify-self:center;margin-top:10%"]       
    [HH.p
    [style"font: 40px Tahoma, Helvetica, Arial, Sans-Serif;text-align: center;color:orange;text-shadow: 0px 2px 3px #555;min-width:100%"] 
        [HH.text $  (fromJustString (myWords !! state.wordCounter))<>" "<> (fromJustString (myWords !! (state.wordCounter+1)))<>" "<> (fromJustString (myWords !! (state.wordCounter+2)))]
      ,HH.input
        [ HP.id_ "inp",
        HE.onValueChange \s -> Just (SendInput s),
        style " color:white; height:50px;width:150px; margin-left:20%; margin-top:5%; margin-bottom:10%;font-size:24px;border-color:orange;background-color:transparent;"
         ]
    ,HH.p
    [style"color:yellow;font:40px Comic Sans;min-width:300px;text-align:center;"] 
        [ HH.text $   show state.timer <>" seconds left"]
    ,HH.p
    [style"color:lightblue;font:24px Comic Sans;min-width:300px;"] 
        [ HH.text $  " WPM: "<> show state.wpm]
    ,HH.p
    [style"color:lightgreen;font:24px Comic Sans;min-width:300px;"] 
        [ HH.text $  "  You Typed:  " <> show state.input <> " "<> show (state.wrongWordCounter) <> " wrong words"]
    ]
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



handleAction :: forall cs o m.MonadEffect m => MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of 
  SendInput s ->
    do
    state<-H.get
    if state.timerIsRunning==false
      then
        do
        mynowtime <- liftEffect nowTime
        H.modify_ \st -> st { timerIsRunning= true,myFirstTime=Just mynowtime}
        _ <- H.subscribe' \sid->
          ES.affEventSource \emitter -> do
            _ <- Aff.forkAff $ repeatAction emitter 1000.0 (Decrement sid) 
            pure mempty
        pure unit
      else 
        pure unit
  
    mynowtime <- liftEffect nowTime
    let timeDifference = lift2 diff (Just mynowtime) state.myFirstTime
    H.modify_ \st -> st { wordCounter= st.wordCounter + 1,input = s, myText=fromJustString (myWords !!st.wordCounter)}
    H.modify_ \st -> st { myTimeNow = Just mynowtime, timeDifference = timeDifference, wpm = calcWPM st.wordCounter timeDifference }
    H.modify_ \st -> st { wrongWordCounter=st.wrongWordCounter+incrementor st.input st.myText}
    _<-liftEffect $ cleanInputBox unit
    pure unit
      
      
  Update -> do
    log("heh")
  Decrement sid -> do
     state <- H.get
     if state.timer>0 
     then  H.modify_ (\st -> st { timer = st.timer - 1 })
      else 
        do 
        unsubscribe sid
        _<-liftEffect $ disableInputBox unit
        pure unit

        





myParagraph :: String
myParagraph = "The world of Dark Souls is a world of cycles. Kingdoms rise and fall, ages come and go, and even time can end and restart as the flame fades and is renewed. These cycles are linked to the First Flame, a mysterious manifestation of life that divides and defines separate states such as heat and cold, or life and death. As the First Flame fades, these differences also begin to fade, such as life and death having little distinction, and humans becoming Undead. The onset of an Age of Dark, the time when the First Flame has fully died, is marked by endless nights, rampant undeath, time, space, and reality breaking down, lands collapsing and converging on one another, people mutating into monsters, darkness covering the world, and the Gods losing their power. To avoid this and prolong the Age of Fire, the bearer of a powerful soul must 'link' themselves to the First Flame, becoming the fuel for another age. If this is not done, the First Flame will eventually die, and an Age of Dark will begin."

myWords :: Array String
myWords = split (Pattern " ") myParagraph

myarrayedstring :: String
myarrayedstring = joinWith "," myWords

repeatAction :: Emitter Aff Action -> Number -> Action -> Aff Unit
repeatAction emitter t action = aux
  where
  aux = do
    Aff.delay (Milliseconds t)
    ES.emit emitter action
    aux



   

  
