module App.MainApp where

import Prelude

import CSS (Float, StyleM, color, height, white, width)
import CSS.Color (red, green)
import Control.Apply (lift2)
import DOM.HTML.Indexed (HTMLimg)
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
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Now (nowTime)
import Halogen (SubscriptionId, liftEffect, unsubscribe)
import Halogen as H
import Halogen.HTML (source, style_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (height, style)
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (Emitter)
import Halogen.Query.EventSource as ES
import SupJS (changeParticleSpeed, cleanInputBox, disableInputBox, resizeMagic)
import Web.DOM.NonElementParentNode (getElementById)









type State = {  
              wrongWordCounter::Int,
              wordCounter::Int,
              myTimeNow:: Maybe Time,
              myFirstTime:: Maybe Time,
              timeDifference::Maybe Seconds,
              timer::Int,
              timerIsRunning::Boolean,
              wpm::Maybe Number,
              zombiePosition::Number,
              particlesWidth::Number}

data Action = Update | SendInput String | Decrement SubscriptionId


fromJustString :: Maybe String -> String
fromJustString Nothing = ""
fromJustString (Just s) = s

fromJustNumber :: Maybe Number -> Number
fromJustNumber Nothing = 1.0
fromJustNumber (Just s) = s

calcWPM:: Int-> Maybe Seconds -> Maybe Number
calcWPM wordsCount (Just(Seconds sec)) = Just( ((toNumber wordsCount) / sec) * toNumber 60 )
calcWPM wordsCount Nothing = Nothing

calcWPMTimer::Number-> Number -> Number
calcWPMTimer wordsCount time = (wordsCount)/((60.0-time))*60.0

sixtySec::Int -> Seconds
sixtySec int = Seconds (toNumber int)

component :: forall q i o m. MonadEffect m => MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> {wpm:Nothing,
    timerIsRunning:false,
    timer:60, wordCounter: 0, 
    wrongWordCounter: 0,
    myTimeNow: Nothing,
    myFirstTime: Nothing,
    timeDifference:Nothing,
    zombiePosition: 350.0,
    particlesWidth:350.0}
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
    [HH.div  
    [style"display:inline-flex;width:100%;justify-self:center;margin-top:10%;background-color:transparent;"]
    [HH.div
    [style("position:relative;display:flex; flex-wrap:wrap;justify-content:center;justify-self:center;left:0px;top:50px")]
    [HH.img
    [HP.src  "images/mage.gif"
    ,HP.height 150
    ,HP.width 200]]
    ,HH.div
    [style("position:relative;display:flex; flex-wrap:wrap;justify-content:center;justify-self:center;left:"<>show state.zombiePosition<>"px;")]
    [HH.img
    [HP.src  "images/zombie-pve.gif"
    ,HP.height 200
    ,HP.width 150]]]
        ]  
    ,HH.div
    [style "width:100%; background-color:#2c2f33;display:flex; flex-wrap:wrap;justify-content:center;justify-self:center;margin-top:10%"]
       
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
        [ HH.text $  "  Correct words: " <> show (state.wordCounter-state.wrongWordCounter) <>" " <> " Wrong words: "<> show (state.wrongWordCounter)]
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

zombiePushValue :: forall t8. Eq t8 => t8 -> t8 -> Number
zombiePushValue input word
    | input == word = 2.0
    | otherwise = -4.0



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
    let myNewWordCounter = state.wordCounter + 1
    let timeDifference = lift2 diff (Just mynowtime) state.myFirstTime
    let myText =fromJustString (myWords !!state.wordCounter)
    let myNewWrongWordCounter=state.wrongWordCounter+incrementor s myText
    let myNewWPM = calcWPM (myNewWordCounter-myNewWrongWordCounter) timeDifference
    let myNewZombiePosition = state.zombiePosition+zombiePushValue s myText
    let newMagic=(resizeMagic (state.zombiePosition))
    pure unit
    H.modify_ \st -> st {
                         wordCounter= myNewWordCounter,
                         myTimeNow = Just mynowtime,
                         timeDifference = timeDifference,
                         wpm = myNewWPM,
                         wrongWordCounter=myNewWrongWordCounter,
                         zombiePosition= myNewZombiePosition}                   
    _<-liftEffect $ cleanInputBox unit
    pure unit
    
    

      
      
  Update -> do
    log("heh")
  Decrement sid -> do
     state <- H.get
     let newSpeed=(changeParticleSpeed (fromJustNumber state.wpm))  
     let newMagic=(resizeMagic (state.zombiePosition))            
     if (state.timer>0) && (state.zombiePosition>(-50.0))
     then
      H.modify_ (\st -> st { timer = st.timer - 1,zombiePosition=st.zombiePosition-8.0,
                            wpm= Just(calcWPMTimer (((toNumber state.wordCounter))-(toNumber state.wrongWordCounter)) (toNumber st.timer))})
      else 
        do 
        unsubscribe sid
        _<-liftEffect $ disableInputBox unit
        let timeDifference = sixtySec 60
        H.modify_\st->st{wpm=calcWPM (st.wordCounter-st.wrongWordCounter) (Just(timeDifference))}
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



   

  
