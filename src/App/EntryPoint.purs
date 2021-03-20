
module App.EntryPoint where

import Prelude

import App.MainApp (handleAction)
import CSS (StyleM, color)
import CSS.Color (red, green)
import Control.Apply (lift2)
import Control.Monad.State (class MonadState)
import Data.Array ((!!))
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
import Halogen (Component, HalogenM, SubscriptionId, liftEffect, unsubscribe)
import Halogen (SubscriptionId, liftEffect, unsubscribe)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (style)
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (Emitter)
import Halogen.Query.EventSource as ES
import SupJS (changeParticleSpeed, cleanInputBox, disableInputBox, resizeMagic)

data State = PVE PveState  | PVP | Entry

type PveState={  
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

updatePVE::(PveState->PveState)-> State->State
updatePVE fn (PVE state) = (PVE (fn state))
updatePVE fn state = state 


initialPVEstate::PveState
initialPVEstate = {wpm:Nothing,
    timerIsRunning:false,
    timer:60, wordCounter: 0, 
    wrongWordCounter: 0,
    myTimeNow: Nothing,
    myFirstTime: Nothing,
    timeDifference:Nothing,
    zombiePosition: 350.0,
    particlesWidth:350.0}


data Action = ActionEntry ActionEntry | ActionPVE ActionPVE 


data ActionEntry = RunPVE | RunPVP  
data ActionPVE =  RunEntry | Update | SendInput String | Decrement SubscriptionId

--entryComponent :: forall t177 t178 t198 t201. Component HTML t201 t198 t178 t177
entryComponent :: forall t400 t401 t423 t426. MonadEffect t400 => MonadAff t400 => Component HTML t426 t423 t401 t400
entryComponent =
  H.mkComponent
    { initialState: \_ -> (Entry)
    , render
    , eval: H.mkEval $ H.defaultEval {handleAction = handleActionPicker }
    }
    where
    handleActionPicker (ActionEntry actionEntry) = handleActionEntry actionEntry
    handleActionPicker (ActionPVE actionPve) = handleActionPVE actionPve
    handleActionEntry = case _ of 
        RunPVE -> do 
         H.put (PVE initialPVEstate)
         pure unit       
        RunPVP -> do
                log("heh")
    
    handleActionPVE = case _ of
        SendInput s ->
            do
            state<-H.get

            case state of
              PVE pveState ->do
                log(show pveState.timerIsRunning)
                if pveState.timerIsRunning==false
                then
                    do
                    mynowtime <- liftEffect nowTime
                    H.modify_ $ updatePVE $ \st->st{ timerIsRunning= true,myFirstTime=Just mynowtime}
                    _ <- H.subscribe' \sid->
                      ES.affEventSource \emitter -> do
                        _ <- Aff.forkAff $ repeatAction emitter 1000.0 (ActionPVE (Decrement sid)) 
                        pure mempty
                    pure unit
                else 
                    pure unit
            
                mynowtime <- liftEffect nowTime
                let myNewWordCounter = pveState.wordCounter + 1
                let timeDifference = lift2 diff (Just mynowtime) pveState.myFirstTime
                let myText =fromJustString (myWords !!pveState.wordCounter)
                let myNewWrongWordCounter=pveState.wrongWordCounter+incrementor s myText
                let myNewWPM = calcWPM (myNewWordCounter-myNewWrongWordCounter) timeDifference
                let myNewZombiePosition = pveState.zombiePosition+zombiePushValue s myText
                let newMagic=(resizeMagic (pveState.zombiePosition))
                pure unit
                H.modify_ $ updatePVE \st -> st{
                                    wordCounter= myNewWordCounter,
                                    myTimeNow = Just mynowtime,
                                    timeDifference = timeDifference,
                                    wpm = myNewWPM,
                                    wrongWordCounter=myNewWrongWordCounter,
                                    zombiePosition= myNewZombiePosition}                   
                _<-liftEffect $ cleanInputBox unit
                pure unit
              _ -> do
                pure unit
            
        Update -> do
         log("heh")
        Decrement sid -> do
            state <- H.get
            case state of
              PVE pveState ->do
                log(show pveState.timer)
                let newSpeed=(changeParticleSpeed (fromJustNumber pveState.wpm))  
                let newMagic=(resizeMagic (pveState.zombiePosition))
                let newtimer = pveState.timer - 1            
                if (pveState.timer>0) && (pveState.zombiePosition>(-50.0))
                then
                do
                    let newZombiePosition = pveState.zombiePosition-8.0
                    H.modify_ $ updatePVE \st-> st{ timer = newtimer,zombiePosition=newZombiePosition,
                                            wpm= Just(calcWPMTimer (((toNumber pveState.wordCounter))-(toNumber pveState.wrongWordCounter)) (toNumber newtimer))}
                    else 
                        do 
                        unsubscribe sid
                        _<-liftEffect $ disableInputBox unit
                        let timeDifference = sixtySec 60
                        H.modify_ $ updatePVE $ \st->st{wpm=calcWPM (pveState.wordCounter-pveState.wrongWordCounter) (Just(timeDifference))}
                        pure unit
              _->do
               pure unit
        RunEntry -> pure unit

    render (Entry) =
     HH.div
        [style "width:50%; background-color:#2c2f33; display:grid; justify-content:center;allign-items:center;"]
        [HH.div
        [style "width:100%; background-color:transparent;"]
        [HH.button [ HE.onClick \_ -> Just (ActionEntry RunPVE) ][ HH.text "Play PVE" ]
        ],
        HH.div
        [style "width:100%; background-color:#2c2f33;"]
        [HH.button [ HE.onClick \_ -> Just (ActionEntry RunPVP) ][ HH.text "Play PVP" ]
            ]
        ]
    render (PVP) =
     HH.div
        [style "width:50%; background-color:#2c2f33;"]
        [
        ]
    render (PVE initialPVEstate)  =
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
        [style("position:relative;display:flex; flex-wrap:wrap;justify-content:center;justify-self:center;left:"<>show initialPVEstate.zombiePosition<>"px;")]
        [HH.img
        [HP.src  "images/zombie-pve.gif"
        ,HP.height 200
        ,HP.width 150]]]
            ]  
        ,HH.div
        [style "width:100%; background-color:#2c2f33;display:flex; flex-wrap:wrap;justify-content:center;justify-self:center;margin-top:10%"]
        
        [HH.p
        [style"font: 40px Tahoma, Helvetica, Arial, Sans-Serif;text-align: center;color:orange;text-shadow: 0px 2px 3px #555;min-width:100%"] 
            [HH.text $  (fromJustString (myWords !! initialPVEstate.wordCounter))<>" "<> (fromJustString (myWords !! (initialPVEstate.wordCounter+1)))<>" "<> (fromJustString (myWords !! (initialPVEstate.wordCounter+2)))]
        ,HH.input
            [ HP.id_ "inp",
            HE.onValueChange \s -> Just (ActionPVE (SendInput s)),
            style " color:white; height:50px;width:150px; margin-left:20%; margin-top:5%; margin-bottom:10%;font-size:24px;border-color:orange;background-color:transparent;"
            ]
        ,HH.p
        [style"color:yellow;font:40px Comic Sans;min-width:300px;text-align:center;"] 
            [ HH.text $   show initialPVEstate.timer <>" seconds left"]
        ,HH.p
        [style"color:lightblue;font:24px Comic Sans;min-width:300px;"] 
            [ HH.text $  " WPM: "<> show initialPVEstate.wpm]
        ,HH.p
        [style"color:lightgreen;font:24px Comic Sans;min-width:300px;"] 
            [ HH.text $  "  Correct words: " <> show (initialPVEstate.wordCounter-initialPVEstate.wrongWordCounter)<>" " <> " Wrong words: "<> show (initialPVEstate.wrongWordCounter)]
        ]
  ]


--               HANDLERS -----------------------------------------------------------------------------------------------------
  







-- pure functions --------------------------------------------------------------------------------------------------------------------

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
