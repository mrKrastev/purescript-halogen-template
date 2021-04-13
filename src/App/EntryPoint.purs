
module App.EntryPoint where

import Prelude

import CSS (StyleM, color)
import CSS.Color (red, green)
import Control.Apply (lift2)
import Control.Monad.State (class MonadState)
import Data.Argonaut (JsonDecodeError, decodeJson, encodeJson, parseJson, printJsonDecodeError, stringify)
import Data.Array ((!!))
import Data.DateTime (Time)
import Data.Either (Either(..))
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe(..))
import Data.Number.Format (precision, toStringWith)
import Data.String (Pattern(..), joinWith, split)
import Data.String as String
import Data.Time (diff)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect.Aff (Aff, Milliseconds(..), delay, error)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Now (nowTime)
import EitherHelpers (mapLeft, (<|||>))
import Halogen (Component, HalogenM, SubscriptionId, liftEffect, unsubscribe)
import Halogen (SubscriptionId, liftEffect, unsubscribe)
import Halogen as H
import Halogen.HTML (HTML, elementNS)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (style)
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (Emitter)
import Halogen.Query.EventSource as ES
import SupJS (startGame, changeParticleSpeed, changeParticleSpeedandWidth, cleanInputBox, disableInputBox, fixPVPmagic, fixPVPmagicPositioning, renderMagicJs, renderMagicJsPVP, resizeMagic, changeParticleSpeed2)
import WSListener (setupWSListener)
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WS

data State = PVE PveState  | PVP PvpState | Entry EntryState

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

type EntryState={  
              name::Maybe String
              }
              
type PvpState={  
              wrongWordCounter::Int,
              wordCounter::Int,
              myTimeNow:: Maybe Time,
              myFirstTime:: Maybe Time,
              timeDifference::Maybe Seconds,
              timer::Int,
              timerIsRunning::Boolean,
              wpm::Maybe Number,
              enemyHp::Number,
              playerHp::Number,
              particlesWidth::Number,
              enemyWPM::Maybe Number,
              webSocket::WebSocket,
              playerName::String,
              enemyName::String,
              enemyCorrectWords::Int,
              notInitialized::Boolean,
              readyTimeoutTimer::Int,
              showGameEndModal::Boolean}
              

updatePVE::(PveState->PveState)-> State->State
updatePVE fn (PVE state) = (PVE (fn state))
updatePVE fn state = state 

updatePVP::(PvpState->PvpState)-> State->State
updatePVP fn (PVP state) = (PVP (fn state))
updatePVP fn state = state 

updateName::(EntryState->EntryState)-> State->State
updateName fn (Entry state) = (Entry (fn state))
updateName fn state = state


initialPVEstate::PveState
initialPVEstate = {wpm:Nothing,
    timerIsRunning:false,
    timer:60, wordCounter: 0, 
    wrongWordCounter: 0,
    myTimeNow: Nothing,
    myFirstTime: Nothing,
    timeDifference:Nothing,
    zombiePosition: 350.0,
    particlesWidth:400.0}

initialEntrystate::EntryState
initialEntrystate = {name:Nothing}

initialPVPstate::WebSocket->String->PvpState
initialPVPstate webSocket name = {wrongWordCounter:0,
              wordCounter:0,
              myTimeNow:Nothing,
              myFirstTime:Nothing,
              timeDifference:Nothing,
              timer:2,
              timerIsRunning:false,
              wpm:Nothing,
              enemyHp:3.0,
              playerHp:3.0,
              particlesWidth:350.0,
              enemyWPM:Nothing,
              webSocket,
              playerName:name,
              enemyName:"Connecting...",
              enemyCorrectWords:0,
              notInitialized:true,
              readyTimeoutTimer:2,
              showGameEndModal:false}


data Action = ActionEntry ActionEntry | ActionPVE ActionPVE |ActionPVP ActionPVP 


data ActionEntry = RunPVE | RunPVP String  | SetName String
data ActionPVE =  RunEntry | Update | SendInput String | Decrement SubscriptionId
data ActionPVP=  RunEntrypvp | Updatepvp SubscriptionId | SendInputpvp String |DecrementInitialTimer SubscriptionId | Decrementpvp SubscriptionId | ReceiveMessage String | UpdatePlayer2 (Maybe Number) Int | SetPlayerConnected String

--entryComponent :: forall t177 t178 t198 t201. Component HTML t201 t198 t178 t177
entryComponent :: forall t400 t401 t423 t426. MonadEffect t400 => MonadAff t400 => Component HTML t426 t423 t401 t400
entryComponent =
  H.mkComponent
    { initialState: \_ -> (Entry initialEntrystate)
    , render
    , eval: H.mkEval $ H.defaultEval {handleAction = handleActionPicker }
    }
    where
    handleActionPicker (ActionEntry actionEntry) = handleActionEntry actionEntry
    handleActionPicker (ActionPVE actionPve) = handleActionPVE actionPve
    handleActionPicker (ActionPVP actionPvp) = handleActionPVP actionPvp
    handleActionEntry = case _ of 
        RunPVE -> do 
         H.put (PVE initialPVEstate)
         pure unit   
         _<-liftEffect $ renderMagicJs unit
         pure unit  
        RunPVP name -> do
            ws <- liftEffect $ WS.create "ws://localhost:3000" []
            liftAff $ delay (Milliseconds 100.0) -- allow ws to initialise
            void $ H.subscribe $
             ES.affEventSource \ emitter -> do
             fiber <- Aff.forkAff $ do
                setupWSListener ws (\msg -> ES.emit emitter (ActionPVP $ ReceiveMessage msg))
             pure $ ES.Finalizer do
                Aff.killFiber (error "Event source finalized") fiber
            liftEffect $ WS.sendString ws $ stringify $ encodeJson {name}
            H.put (PVP (initialPVPstate ws name))
            void $ liftEffect $ fixPVPmagic unit
            pure unit
        SetName s -> H.modify_ $ updateName $ \st->st{name=Just(s)}
            

    handleActionPVP = case _ of 
       SendInputpvp s ->
            do
            state<-H.get
            case state of
              PVP pvpState ->do
                log(show pvpState.timerIsRunning)
                if pvpState.timerIsRunning==false
                then
                pure unit
                else 
                    pure unit
            
                mynowtime <- liftEffect nowTime
                let myNewWordCounter = pvpState.wordCounter + 1
                let timeDifference = lift2 diff (Just mynowtime) pvpState.myFirstTime
                let myText =fromJustString (myWords !!pvpState.wordCounter)
                let myNewWrongWordCounter=pvpState.wrongWordCounter+incrementor s myText
                let myNewWPM = calcWPM (myNewWordCounter-myNewWrongWordCounter) timeDifference
                let correctWords = myNewWordCounter-myNewWrongWordCounter
                let particlesWidthUpdate = pvpState.particlesWidth+magicPushCalculator s myText
                --let particlesWidthUpdate = calculateMagicPush  (correctWords) (pvpState.enemyCorrectWords) (pvpState.particlesWidth)
                let newMagic=(resizeMagic (pvpState.particlesWidth))
                pure unit
                H.modify_ $ updatePVP \st -> st{
                                    wordCounter= myNewWordCounter,
                                    myTimeNow = Just mynowtime,
                                    timeDifference = timeDifference,
                                    wpm = myNewWPM,
                                    wrongWordCounter=myNewWrongWordCounter,
                                    particlesWidth=particlesWidthUpdate}
                let player2WPM = myNewWPM
                --let correctWords = myNewWordCounter-myNewWrongWordCounter
                --liftEffect $ WS.sendString pvpState.webSocket $ stringify $ encodeJson {player2WPM,correctWords}
                                      
                _<-liftEffect $ cleanInputBox unit
                pure unit
              _ -> do
                pure unit
            
       Updatepvp sid ->do
            state <- H.get
            case state of
             PVP pvpState -> do
              let player2WPM = pvpState.wpm
              let correctWords = (pvpState.wordCounter - pvpState.wrongWordCounter)
              liftEffect $ WS.sendString pvpState.webSocket $ stringify $ encodeJson {player2WPM,correctWords}
              let newMagic=(resizeMagic (pvpState.particlesWidth))
              pure unit
             _ -> pure unit
       Decrementpvp sid -> do
            state <- H.get
            case state of
              PVP pvpState ->do 
                let newtimer = pvpState.timer - 1        
                if (pvpState.timer>0) && ((pvpState.particlesWidth>(-200.0) && (pvpState.particlesWidth<(900.0))))
                then
                do
                    
                    let newParticlesWidth = pvpState.particlesWidth
                    
                    H.modify_ $ updatePVP \st-> st{ timer = newtimer,particlesWidth=newParticlesWidth,
                                            wpm= Just(calcWPMTimer (((toNumber pvpState.wordCounter))-(toNumber pvpState.wrongWordCounter)) (toNumber newtimer))}
                   -- liftEffect $ WS.sendString pvpState.webSocket $ stringify $ encodeJson {pvp,correctWords}
                    void $ liftEffect $ changeParticleSpeed (fromJustNumber pvpState.wpm)
                   -- void $ liftEffect $ (resizeMagic (pvpState.particlesWidth))   
                    else 
                        do 
                        unsubscribe sid
                        H.modify_ $ updatePVP $ \st->st{showGameEndModal=true}
                        _<-liftEffect $ disableInputBox unit
                        pure unit
              _->do
               pure unit
       DecrementInitialTimer sid -> do
            state <- H.get
            case state of
              PVP pvpState ->do 
                let newTimeoutTimer = pvpState.readyTimeoutTimer - 1        
                if (pvpState.readyTimeoutTimer>0)
                then
                do
                    H.modify_ $ updatePVP \st-> st{ readyTimeoutTimer = newTimeoutTimer}  
                    else 
                        do 
                        unsubscribe sid
                        mynowtime <- liftEffect nowTime
                        H.modify_ $ updatePVP $ \st->st{ timerIsRunning= true,myFirstTime=Just mynowtime}
                        _ <- H.subscribe' \sid2->
                         ES.affEventSource \emitter -> do
                         _ <- Aff.forkAff $ repeatAction emitter 1000.0 (ActionPVP (Decrementpvp sid2)) 
                         pure mempty
                        _ <- H.subscribe' \sid3->
                         ES.affEventSource \emitter -> do
                         _ <- Aff.forkAff $ repeatAction emitter 100.0 (ActionPVP (Updatepvp sid3)) 
                         pure mempty
                        pure unit
                        _<-liftEffect $ startGame unit
                        pure unit
              _->do
               pure unit       
       RunEntrypvp -> pure unit
       ReceiveMessage msg -> do
        case messageToAction msg of
            Left err -> liftEffect $ log err
            Right action -> handleActionPVP action
       UpdatePlayer2 wpm numberOfCorrectWords -> do 
                                                  log("updatePlayer")
                                                  
                                                  state <- H.get
                                                  case state of
                                                      PVP pvpState -> do
                                                       let previousEnemyCorrectWords=pvpState.enemyCorrectWords
                                                       log(show previousEnemyCorrectWords <> "second val :"<> show numberOfCorrectWords)
                                                       let updatedParticlesWidth = pvpState.particlesWidth + (calculateMagicPush previousEnemyCorrectWords numberOfCorrectWords)
                                                       log(show updatedParticlesWidth)
                                                       H.modify_ $ updatePVP $ \st->st{particlesWidth=updatedParticlesWidth,enemyWPM=wpm,enemyCorrectWords=numberOfCorrectWords}
                                                       log(show pvpState.enemyWPM)
                                                       --void $ liftEffect $ changeParticleSpeedandWidth (fromJustNumber pvpState.enemyWPM) (pvpState.particlesWidth)
                                                       -- $ liftEffect $ (resizeMagic (pvpState.particlesWidth))  
                                                       void $ liftEffect $ changeParticleSpeed2 (fromJustNumber pvpState.enemyWPM)
                                                       
                                                      _->do
                                                       pure unit
                                                  pure unit
       SetPlayerConnected enemyID -> do
                                    state <- H.get
                                    H.modify_ $ updatePVP $ \st->st{enemyName=enemyID}
                                    case state of
                                     PVP pvpState ->do
                                      if(pvpState.notInitialized)
                                      then
                                       do
                                       log(pvpState.playerName <> "passed")
                                       let name =  pvpState.playerName
                                       liftEffect $ WS.sendString pvpState.webSocket $ stringify $ encodeJson {name}
                                       H.modify_ $ updatePVP $ \st->st{notInitialized=false}
                                       _ <- H.subscribe' \sid->
                                        ES.affEventSource \emitter -> do
                                        _ <- Aff.forkAff $ repeatAction emitter 1000.0 (ActionPVP (DecrementInitialTimer sid)) 
                                        pure mempty
                                       pure unit
                                       else
                                        log(pvpState.playerName <> "did not pass")
                                     _->do
                                      pure unit
        
    
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
                let newMagic=(resizeMagic (pveState.zombiePosition))
                let newtimer = pveState.timer - 1       
                if (pveState.timer>0) && (pveState.zombiePosition>(-50.0))
                then
                do
                    let newZombiePosition = pveState.zombiePosition-8.0
                    H.modify_ $ updatePVE \st-> st{ timer = newtimer,zombiePosition=newZombiePosition,
                                            wpm= Just(calcWPMTimer (((toNumber pveState.wordCounter))-(toNumber pveState.wrongWordCounter)) (toNumber newtimer))}
                    void $ liftEffect $ changeParticleSpeed (fromJustNumber pveState.wpm)

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

    render (Entry initialEntrystate) =
     HH.div
        [style "width:50%; background-color:#2c2f33; display:grid; justify-content:center;allign-items:center;"]
        [HH.div
        [style "width:100%; background-color:transparent;"]
        [HH.button [ HE.onClick \_ -> Just (ActionEntry RunPVE) ][ HH.text "Play PVE" ]
        ],
        HH.div
        [style "width:100%; background-color:#2c2f33;"]
        [HH.button [ HE.onClick \_ -> Just (ActionEntry (RunPVP $ fromJustString initialEntrystate.name)) ][ HH.text "Play PVP" ]
            ],
        HH.div
        [style "width:100%; background-color:#2c2f33;"]
        [HH.input [ HE.onValueInput \s -> Just(ActionEntry(SetName s)) ]
            ],
        HH.div
        [style "width:100%; background-color:#2c2f33;"]
        [HH.p
        [style"color:yellow;font:40px Comic Sans;min-width:300px;text-align:center;"] 
            [ HH.text $ "Wizard name: " <> fromJustString initialEntrystate.name]]
        ]
    render (PVP initialPVPstate) =
     HH.div
        [style "width:70%; background-color:#2c2f33;"]
        [HH.p
        [style $ timerVisibility initialPVPstate.readyTimeoutTimer] 
            [ HH.text $ "Battle Starts In: " <> show initialPVPstate.readyTimeoutTimer]
          ,HH.div
        [style $ modalCSS initialPVPstate.showGameEndModal][
            HH.p
            [style $ battleOutcomeTitle "draw"] 
            [ HH.text "Draw"],
            HH.div[style"position:relative;width:100%;height:200px;"][
            HH.p
            [style "width:150px;float:left;font-size:40px;color:cyan;text-shadow: 0px 0px 5px #555;margin:10px;padding-left:20px;"] 
            [ HH.text $ "WPM: " <> toStringWith (precision 3 ) (fromJustNumber initialPVPstate.wpm)]
            ,HH.p
            [style "width:150px;float:right;font-size:40px;color:red;text-shadow: 0px 0px 5px #555;margin:10px;padding-right:30px;"] 
            [ HH.text $ "WPM: " <> toStringWith (precision 3 ) (fromJustNumber initialPVPstate.enemyWPM)]
            ],
            HH.div[style"position:relative;width:100%;height:200px;"][
            HH.p
            [style "width:150px;float:left;font-size:40px;color:cyan;text-shadow: 0px 0px 5px #555;margin:10px;padding-left:20px;"] 
            [ HH.text $ "Correct Words: " <> show (initialPVPstate.wordCounter-initialPVPstate.wrongWordCounter)]
            ,HH.p
            [style "width:150px;float:right;font-size:40px;color:red;text-shadow: 0px 0px 5px #555;margin:10px;padding-right:30px;"] 
            [ HH.text $ "Correct Words: " <> show initialPVPstate.enemyCorrectWords]
            ]
        ]
          ,HH.div  
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
        [style("position:absolute;display:flex; flex-wrap:wrap;justify-content:center;justify-self:center;right:15%;top:32%;")]
        [HH.img
        [HP.src  "images/player2-mage.gif"
        ,HP.height 150
        ,HP.width 200]]]
        ,HH.div
        [style("position:absolute;display: inline-flex; flex-wrap:nowrap; width:60%;justify-content:space-between;justify-self:center;top:50%")]
        [HH.p
        [style"position:relative;font: 40px Tahoma, Helvetica, Arial, Sans-Serif;text-align: center;color:yellow;text-shadow: 0px 2px 3px #555;"] 
            [HH.text $  (initialPVPstate.playerName)]
        ,HH.p
        [style"position:relative;font: 40px Tahoma, Helvetica, Arial, Sans-Serif;text-align: center;color:yellow;text-shadow: 0px 2px 3px #555;"] 
            [HH.text $  (initialPVPstate.enemyName)]
        ]
        ]
        ,
        HH.div
        [style "width:100%; background-color:#2c2f33;display:flex; flex-wrap:wrap;justify-content:center;justify-self:center;margin-top:10%"]
        [HH.p
        [style"font: 40px Tahoma, Helvetica, Arial, Sans-Serif;text-align: center;color:orange;text-shadow: 0px 2px 3px #555;min-width:100%"] 
            [HH.text $  (fromJustString (myWords !! initialPVPstate.wordCounter))<>" "<> (fromJustString (myWords !! (initialPVPstate.wordCounter+1)))<>" "<> (fromJustString (myWords !! (initialPVPstate.wordCounter+2)))]
        ,HH.input
            [ HP.id_ "inp",
            HE.onValueChange \s -> Just (ActionPVP (SendInputpvp s)),
            style " color:white; height:50px;width:150px; margin-left:20%; margin-top:5%; margin-bottom:10%;font-size:24px;border-color:orange;background-color:transparent;"
            ]
        ,HH.p
        [style"color:yellow;font:40px Comic Sans;min-width:300px;text-align:center;"] 
            [ HH.text $   show initialPVPstate.timer <>" seconds left"]
        ,HH.p
        [style"color:lightblue;font:24px Comic Sans;min-width:300px;"] 
            [ HH.text $  " WPM: "<> show initialPVPstate.wpm]
        ,HH.p
        [style"color:lightgreen;font:24px Comic Sans;min-width:300px;"] 
            [ HH.text $  "  Correct words: " <> show (initialPVPstate.wordCounter-initialPVPstate.wrongWordCounter)<>" " <> " Wrong words: "<> show (initialPVPstate.wrongWordCounter)]
        ]
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


-- HANDLERS -----------------------------------------------------------------------------------------------------
  



fixPVPmagicPosition :: forall t40. Bind t40 => MonadEffect t40 => t40 Unit
fixPVPmagicPosition = do
                _<-liftEffect $ fixPVPmagicPositioning unit
                pure unit
fixPVPmagicRender :: forall t30. Bind t30 => MonadEffect t30 => t30 Unit
fixPVPmagicRender = do
                _<-liftEffect $ renderMagicJsPVP unit
                pure unit
--CSS kek------------------------------------------------------------------------------------------------------------------------------

modalCSS :: Boolean -> String
modalCSS flag
 | flag == true = "width:50%; height:wrap-content;left:25%;top:20%;z-index:999; position:absolute;background-color:#23272a;display:block;justify-content:space-between;overflow:wrap;"
 | otherwise = "width:50%; height:wrap-content;left:25%;top:20%;z-index:999; position:absolute;background-color:#23272a;display:none;"

battleOutcomeTitle outcome 
 | outcome == "draw" = "width:100%;margin: 50px auto;text-align: center;text-shadow: -1px -1px 0px rgba(255,255,255,0.3), 1px 1px 0px rgba(0,0,0,0.8);color:rgba(75, 119, 190, 1);opacity: 1;font: 700 80px 'Bitter'"
 | otherwise ="margin:width:100%; 50px auto;text-align: center;text-shadow: -1px -1px 0px rgba(255,255,255,0.3), 1px 1px 0px rgba(0,0,0,0.8);color: #333;opacity: 0.4;font: 700 80px 'Bitter'"
-- pure functions --------------------------------------------------------------------------------------------------------------------
calculateMagicPush::Int->Int->Number
calculateMagicPush p1Words p2Words
    | p1Words < p2Words = -10.0
    | otherwise = 0.0
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
    

timerVisibility :: Int -> String
timerVisibility timer
    | timer > 0 = "position:absolute;left:40%;color:yellow;font:40px Comic Sans;min-width:300px;text-align:center;visibility:visible;"
    | otherwise = "position:absolute;left:40%;color:yellow;font:40px Comic Sans;min-width:300px;text-align:center;visibility:hidden;"
incrementor :: forall t8. Eq t8 => t8 -> t8 -> Int
incrementor input word
    | input == word = 0
    | otherwise = 1

zombiePushValue :: forall t8. Eq t8 => t8 -> t8 -> Number
zombiePushValue input word
    | input == word = 2.0
    | otherwise = -4.0
magicPushCalculator :: forall t8. Eq t8 => t8 -> t8 -> Number
magicPushCalculator input word
    | input == word = 10.0
    | otherwise = 0.0

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

-- message types:
type EnemyState = { player2WPM :: Maybe Number, correctWords :: Int }
type ID = { name :: String }

messageToAction :: String -> Either String ActionPVP
messageToAction msg = do
  json <- (parseJson msg) # (describeErr "Failed to parse message as JSON: ")
  (parseSetPlayer json <|||> parseSetIt json) # describeErrs "Failed to decode JSON:\n"
  where
  parseSetPlayer json = do
    ({player2WPM, correctWords} :: EnemyState) <- decodeJson json
    pure (UpdatePlayer2 player2WPM  correctWords)
  parseSetIt json = do
    ({name} :: ID) <- decodeJson json
    pure (SetPlayerConnected name)

  describeErr :: forall b.String -> Either JsonDecodeError b -> Either String b
  describeErr s = mapLeft (\ err -> s <> (printJsonDecodeError err))
  describeErrs :: forall b.String -> Either (Array JsonDecodeError) b -> Either String b
  describeErrs s = mapLeft (\ errs -> s <> String.joinWith "\n" (map printJsonDecodeError errs))

