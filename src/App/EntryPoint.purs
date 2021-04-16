
module App.EntryPoint where

import Prelude

import CSS (StyleM, color)
import CSS.Color (red, green)
import Control.Apply (lift2)
import Control.Monad.State (class MonadState)
import Data.Argonaut (JsonDecodeError, decodeJson, encodeJson, parseJson, printJsonDecodeError, stringify)
import Data.Argonaut as Console
import Data.Array (index, (!!))
import Data.DateTime (Time)
import Data.Either (Either(..))
import Data.Int (fromNumber, toNumber)
import Data.Int.Parse (parseInt, toRadix)
import Data.Maybe (Maybe(..))
import Data.Number.Format (precision, toStringWith)
import Data.String (Pattern(..), joinWith, split)
import Data.String as String
import Data.Time (diff)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, error)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Now (nowTime)
import Effect.Random (randomInt)
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
import Run (runPure)
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
              particlesWidth::Number,
              showGameEndModal::Boolean,
              combatOutcome::String,
              textNo::Int}

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
              particlesWidth::Number,
              enemyWPM::Maybe Number,
              webSocket::WebSocket,
              playerName::String,
              enemyName::String,
              enemyCorrectWords::Int,
              notInitialized::Boolean,
              readyTimeoutTimer::Int,
              showGameEndModal::Boolean,
              combatOutcome::String,
              hasOpponent::Boolean,
              opponentID::Maybe String,
              myID::String,
              textNo::Int}
              

updatePVE::(PveState->PveState)-> State->State
updatePVE fn (PVE state) = (PVE (fn state))
updatePVE fn state = state 

updatePVP::(PvpState->PvpState)-> State->State
updatePVP fn (PVP state) = (PVP (fn state))
updatePVP fn state = state 

updateName::(EntryState->EntryState)-> State->State
updateName fn (Entry state) = (Entry (fn state))
updateName fn state = state


initialPVEstate::Int->PveState
initialPVEstate textNum = {wpm:Nothing,
    timerIsRunning:false,
    timer:60, wordCounter: 0, 
    wrongWordCounter: 0,
    myTimeNow: Nothing,
    myFirstTime: Nothing,
    timeDifference:Nothing,
    zombiePosition: 350.0,
    particlesWidth:400.0,
    showGameEndModal:false,
    combatOutcome:"Win",
    textNo:textNum}

initialEntrystate::EntryState
initialEntrystate = {name:Nothing}

initialPVPstate::WebSocket->String->String->Int->PvpState
initialPVPstate webSocket name id textNumber = {wrongWordCounter:0,
              wordCounter:0,
              myTimeNow:Nothing,
              myFirstTime:Nothing,
              timeDifference:Nothing,
              timer:60,
              timerIsRunning:false,
              wpm:Nothing,
              particlesWidth:350.0,
              enemyWPM:Nothing,
              webSocket,
              playerName:name,
              enemyName:"Connecting...",
              enemyCorrectWords:0,
              notInitialized:true,
              readyTimeoutTimer:30,
              showGameEndModal:false,
              combatOutcome:"Draw",
              hasOpponent:false,
              opponentID:Nothing,
              myID:id,
              textNo:textNumber}


data Action = ActionEntry ActionEntry | ActionPVE ActionPVE |ActionPVP ActionPVP 


data ActionEntry = RunPVE | RunPVP String  | SetName String
data ActionPVE =  RunEntry | Update | SendInput String | Decrement SubscriptionId
data ActionPVP=  RunEntrypvp | Updatepvp SubscriptionId | SendInputpvp String |DecrementInitialTimer SubscriptionId | Decrementpvp SubscriptionId | ReceiveMessage String | UpdatePlayer2 (Maybe Number) Int String | SetPlayerConnected String String Int 

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
         random <- liftEffect $ randomInt 0 4
         let textNumber = fromJustInt((parseInt (show random) (toRadix 10)))
         H.put (PVE (initialPVEstate (textNumber)))
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
            myid <- liftEffect $ generateRandomNumber
            random <- liftEffect $ randomInt 0 4
            let textNumber = fromJustInt((parseInt (show random) (toRadix 10)))
            H.put (PVP (initialPVPstate ws name (show myid) textNumber))
            let playerID = show myid
            liftEffect $ WS.sendString ws $ stringify $ encodeJson {playerID,name,textNumber}
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
                let myText =fromJustString ((myWords pvpState.textNo) !!pvpState.wordCounter)
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
              let playerID=pvpState.myID
              liftEffect $ WS.sendString pvpState.webSocket $ stringify $ encodeJson {playerID,player2WPM,correctWords}
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
                        let myCorrectWords =pvpState.wordCounter-pvpState.wrongWordCounter
                        let outcome = decideWinner myCorrectWords pvpState.enemyCorrectWords (wpmSetup pvpState.wpm) (wpmSetup pvpState.enemyWPM)
                        H.modify_ $ updatePVP $ \st->st{showGameEndModal=true,combatOutcome=outcome}
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
       UpdatePlayer2 wpm numberOfCorrectWords dataID -> do 
                                                  log("updatePlayer")
                                                  
                                                  state <- H.get
                                                  case state of
                                                     PVP pvpState -> do
                                                      if dataID==(fromJustString pvpState.opponentID) then do
                                                    
                                                       let previousEnemyCorrectWords=pvpState.enemyCorrectWords
                                                       log(show previousEnemyCorrectWords <> "second val :"<> show numberOfCorrectWords)
                                                       let updatedParticlesWidth = pvpState.particlesWidth + (calculateMagicPush previousEnemyCorrectWords numberOfCorrectWords)
                                                       log(show updatedParticlesWidth)
                                                       H.modify_ $ updatePVP $ \st->st{particlesWidth=updatedParticlesWidth,enemyWPM=wpm,enemyCorrectWords=numberOfCorrectWords}
                                                       log(show pvpState.enemyWPM)
                                                       --void $ liftEffect $ changeParticleSpeedandWidth (fromJustNumber pvpState.enemyWPM) (pvpState.particlesWidth)
                                                       -- $ liftEffect $ (resizeMagic (pvpState.particlesWidth))  
                                                       void $ liftEffect $ changeParticleSpeed2 (fromJustNumber pvpState.enemyWPM)
                                                      else pure unit
                                                     _->do
                                                       pure unit
                                                  pure unit
       SetPlayerConnected id opponentName textId  -> do
                                    state <- H.get
                                    case state of
                                     PVP pvpState ->do
                                      if(pvpState.notInitialized && pvpState.hasOpponent==false)
                                      then
                                       do
                                       H.modify_ $ updatePVP $ \st->st{enemyName=opponentName,opponentID=Just id, hasOpponent=true}
                                       if(Just textId == Just pvpState.textNo) then do
                                         pure unit
                                       else H.modify_ $ updatePVP $ \st->st{textNo=textId}
                                       let name =  pvpState.playerName
                                       let playerID =  pvpState.myID
                                       let textNumber =  textId
                                       liftEffect $ WS.sendString pvpState.webSocket $ stringify $ encodeJson {playerID, name,textNumber}
                                       H.modify_ $ updatePVP $ \st->st{notInitialized=false}
                                       _ <- H.subscribe' \sid->
                                        ES.affEventSource \emitter -> do
                                        _ <- Aff.forkAff $ repeatAction emitter 1000.0 (ActionPVP (DecrementInitialTimer sid)) 
                                        pure mempty
                                       pure unit
                                       else pure unit
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
                let myText =fromJustString ((myWords pveState.textNo) !!pveState.wordCounter)
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
                        if pveState.zombiePosition <= -50.0 then do
                          H.modify_ $ updatePVE $ \st->st{combatOutcome="Loss",showGameEndModal=true}
                        else H.modify_ $ updatePVE $ \st->st{showGameEndModal=true}
                        let timeDifference = sixtySec 60
                        H.modify_ $ updatePVE $ \st->st{wpm=calcWPM (pveState.wordCounter-pveState.wrongWordCounter) (Just(timeDifference))}
                        _<-liftEffect $ disableInputBox unit
                        pure unit
              _->do
               pure unit
        RunEntry -> pure unit

    render (Entry entrystate) =
     HH.div
        [style "width:50%; background-color:#2c2f33; display:grid; justify-content:center;allign-items:center;"]
        [HH.div
        [style "width:100%; background-color:transparent;"]
        [HH.button [ HE.onClick \_ -> Just (ActionEntry RunPVE) ][ HH.text "Play PVE" ]
        ],
        HH.div
        [style "width:100%; background-color:#2c2f33;"]
        [HH.button [ HE.onClick \_ -> Just (ActionEntry (RunPVP $ fromJustString entrystate.name)) ][ HH.text "Play PVP" ]
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
    render (PVP myPVPstate) =
     HH.div
        [style "width:70%; background-color:#2c2f33;"]
        [HH.p
        [style $ timerVisibility myPVPstate.readyTimeoutTimer] 
            [ HH.text $ "Battle Starts In: " <> show myPVPstate.readyTimeoutTimer]
          ,HH.div
        [style $ modalCSS myPVPstate.showGameEndModal][
            HH.p
            [style $ battleOutcomeTitle myPVPstate.combatOutcome] 
            [ HH.text myPVPstate.combatOutcome],
            HH.div[style"position:relative;width:100%;height:200px;"][
            HH.p
            [style "width:300px;float:left;font-size:40px;color:cyan;text-shadow: 0px 0px 5px #555;margin:10px;padding-left:20px;"] 
            [ HH.text $ "WPM: " <> toStringWith (precision 3 ) (fromJustNumber myPVPstate.wpm)]
            ,HH.p
            [style "width:300px;float:right;font-size:40px;color:red;text-shadow: 0px 0px 5px #555;margin:10px;padding-right:30px;"] 
            [ HH.text $ "WPM: " <> toStringWith (precision 3 ) (fromJustNumber myPVPstate.enemyWPM)]
            ],
            HH.div[style"position:relative;width:100%;height:200px;"][
            HH.p
            [style "width:300px;float:left;font-size:40px;color:cyan;text-shadow: 0px 0px 5px #555;margin:10px;padding-left:20px;"] 
            [ HH.text $ "Correct Words: " <> show (myPVPstate.wordCounter-myPVPstate.wrongWordCounter)]
            ,HH.p
            [style "width:300px;float:right;font-size:40px;color:red;text-shadow: 0px 0px 5px #555;margin:10px;padding-right:30px;"] 
            [ HH.text $ "Correct Words: " <> show myPVPstate.enemyCorrectWords]
            ],
            HH.div[style"position:relative;width:100%;height:200px;"][
            HH.p
            [style "width:100%;font-size:30px;color:orange;text-shadow: 0px 0px 5px #555;margin:10px;"] 
            [ HH.text $ generateFlavourText (wpmSetup myPVPstate.wpm) (wpmSetup myPVPstate.enemyWPM)]]
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
            [HH.text $  (myPVPstate.playerName <> myPVPstate.myID)]
        ,HH.p
        [style"position:relative;font: 40px Tahoma, Helvetica, Arial, Sans-Serif;text-align: center;color:yellow;text-shadow: 0px 2px 3px #555;"] 
            [HH.text $  (myPVPstate.enemyName<> fromJustString(myPVPstate.opponentID))]
        ]
        ]
        ,
        HH.div
        [style "width:100%; background-color:#2c2f33;display:flex; flex-wrap:wrap;justify-content:center;justify-self:center;margin-top:10%"]
        [HH.p
        [style"font: 40px Tahoma, Helvetica, Arial, Sans-Serif;text-align: center;color:orange;text-shadow: 0px 2px 3px #555;min-width:100%"] 
            [HH.text $  (fromJustString ((myWords myPVPstate.textNo) !! myPVPstate.wordCounter))<>" "<> (fromJustString ((myWords myPVPstate.textNo) !! (myPVPstate.wordCounter+1)))<>" "<> (fromJustString ((myWords myPVPstate.textNo) !! (myPVPstate.wordCounter+2)))]
        ,HH.input
            [ HP.id_ "inp",
            HE.onValueChange \s -> Just (ActionPVP (SendInputpvp s)),
            style " color:white; height:50px;width:150px; margin-left:20%; margin-top:5%; margin-bottom:10%;font-size:24px;border-color:orange;background-color:transparent;"
            ]
        ,HH.p
        [style"color:yellow;font:40px Comic Sans;min-width:300px;text-align:center;"] 
            [ HH.text $   show myPVPstate.timer <>" seconds left"]
        ,HH.p
        [style"color:lightblue;font:24px Comic Sans;min-width:300px;"] 
            [ HH.text $  " WPM: "<> show myPVPstate.wpm]
        ,HH.p
        [style"color:lightgreen;font:24px Comic Sans;min-width:300px;"] 
            [ HH.text $  "  Correct words: " <> show (myPVPstate.wordCounter-myPVPstate.wrongWordCounter)<>" " <> " Wrong words: "<> show (myPVPstate.wrongWordCounter)]
        ]
        ]
    render (PVE myPVEstate)  =
     HH.div
     [style "width:50%; background-color:#2c2f33;"]
     [HH.div
        [style $ modalCSS myPVEstate.showGameEndModal][
            HH.p
            [style $ pveOutcome myPVEstate.combatOutcome] 
            [ HH.text myPVEstate.combatOutcome],
            HH.div[style"position:relative;width:100%;height:150px;"][
            HH.p
            [style "width:300px;float:left;font-size:30px;color:cyan;text-shadow: 0px 0px 5px #555;margin:10px;padding-left:20px;"] 
            [ HH.text $ "WPM: " <> toStringWith (precision 3 ) (fromJustNumber myPVEstate.wpm)]
            ],
            HH.div[style"position:relative;width:100%;height:150px;"][
            HH.p
            [style "width:300px;float:left;font-size:30px;color:cyan;text-shadow: 0px 0px 5px #555;margin:10px;padding-left:20px;"] 
            [ HH.text $ "Correct Words: " <> show (myPVEstate.wordCounter-myPVEstate.wrongWordCounter)]
            ],
            HH.div[style"position:relative;width:100%;height:200px;"][
            HH.p
            [style "width:100%;font-size:20px;color:orange;text-shadow: 0px 0px 5px #555;margin:10px;"] 
            [ HH.text $ "You typed " <> (toStringWith (precision 3) (fromJustNumber(myPVEstate.wpm)/60.0)) <> " words in a second!" ]
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
        [style("position:relative;display:flex; flex-wrap:wrap;justify-content:center;justify-self:center;left:"<>show myPVEstate.zombiePosition<>"px;")]
        [HH.img
        [HP.src  "images/zombie-pve.gif"
        ,HP.height 200
        ,HP.width 150]]]
            ]  
        ,HH.div
        [style "width:100%; background-color:#2c2f33;display:flex; flex-wrap:wrap;justify-content:center;justify-self:center;margin-top:10%"]
        
        [HH.p
        [style"font: 40px Tahoma, Helvetica, Arial, Sans-Serif;text-align: center;color:orange;text-shadow: 0px 2px 3px #555;min-width:100%"] 
            [HH.text $  (fromJustString ((myWords myPVEstate.textNo) !! myPVEstate.wordCounter))<>" "<> (fromJustString ((myWords myPVEstate.textNo) !! (myPVEstate.wordCounter+1)))<>" "<> (fromJustString ((myWords myPVEstate.textNo) !! (myPVEstate.wordCounter+2)))]
        ,HH.input
            [ HP.id_ "inp",
            HE.onValueChange \s -> Just (ActionPVE (SendInput s)),
            style " color:white; height:50px;width:150px; margin-left:20%; margin-top:5%; margin-bottom:10%;font-size:24px;border-color:orange;background-color:transparent;"
            ]
        ,HH.p
        [style"color:yellow;font:40px Comic Sans;min-width:300px;text-align:center;"] 
            [ HH.text $   show myPVEstate.timer <>" seconds left"]
        ,HH.p
        [style"color:lightblue;font:24px Comic Sans;min-width:300px;"] 
            [ HH.text $  " WPM: "<> show myPVEstate.wpm]
        ,HH.p
        [style"color:lightgreen;font:24px Comic Sans;min-width:300px;"] 
            [ HH.text $  "  Correct words: " <> show (myPVEstate.wordCounter-myPVEstate.wrongWordCounter)<>" " <> " Wrong words: "<> show (myPVEstate.wrongWordCounter)]
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
 | flag == true = "width:50%; height:wrap-content;left:25%;top:15%;z-index:999; position:absolute;background-color:#23272a;display:block;justify-content:space-between;overflow:wrap;"
 | otherwise = "width:50%; height:wrap-content;left:25%;top:15%;z-index:999; position:absolute;background-color:#23272a;display:none;"

battleOutcomeTitle :: String -> String
battleOutcomeTitle outcome 
 | outcome == "Victory" = "width:100%;margin: 50px auto;text-align: center;text-shadow: -1px -1px 0px rgba(255,255,255,0.3), 1px 1px 0px rgba(0,0,0,0.8);color:rgba(30, 130, 76, 1);opacity: 1;font: 700 80px 'Bitter'"
 | outcome == "Defeat" = "width:100%;margin: 50px auto;text-align: center;text-shadow: -1px -1px 0px rgba(255,255,255,0.3), 1px 1px 0px rgba(0,0,0,0.8);color:rgba(150, 40, 27, 1);opacity: 1;font: 700 80px 'Bitter'"
 | otherwise =":width:100%;margin: 50px auto;text-align: center;text-shadow: -1px -1px 0px rgba(255,255,255,0.3), 1px 1px 0px rgba(0,0,0,0.8);color:rgba(75, 119, 190, 1);opacity: 1;font: 700 80px 'Bitter'"

pveOutcome :: String -> String
pveOutcome outcome 
 | outcome == "Win" = "width:100%;margin: 50px auto;text-align: center;text-shadow: -1px -1px 0px rgba(255,255,255,0.3), 1px 1px 0px rgba(0,0,0,0.8);color:rgba(30, 130, 76, 1);opacity: 1;font: 700 80px 'Bitter'"
 | otherwise = "width:100%;margin: 50px auto;text-align: center;text-shadow: -1px -1px 0px rgba(255,255,255,0.3), 1px 1px 0px rgba(0,0,0,0.8);color:rgba(150, 40, 27, 1);opacity: 1;font: 700 80px 'Bitter'"



-- pure functions --------------------------------------------------------------------------------------------------------------------


decideWinner :: forall t113 t115. Ord t113 => Ord t115 => Ord t115 => t113 -> t113 -> t115 -> t115 -> String
decideWinner p1Words p2Words p1WPM p2WPM
    | p1Words>p2Words = "Victory"
    | p1Words<p2Words = "Defeat"
    | p1WPM>p2WPM = "Victory"
    | p1WPM<p2WPM = "Defeat"
    | otherwise = "Draw"
    
generateFlavourText :: Number -> Number -> String
generateFlavourText wpm1 wpm2
 | wpm1 > wpm2 = "You were "<> toStringWith (precision 3) (((wpm1/wpm2)-1.0)*100.0) <> "% faster than your opponent!"
 | wpm1 < wpm2 = "You were "<> toStringWith (precision 3) (((wpm2/wpm1)-1.0)*100.0) <> "% slower than your opponent!"
 | otherwise = "Your magical power levels are matching!"

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

fromJustInt :: Maybe Int -> Int
fromJustInt Nothing = 0
fromJustInt (Just s) = s

wpmSetup :: Maybe Number -> Number
wpmSetup Nothing = 0.0
wpmSetup (Just s) = s

calcWPM:: Int-> Maybe Seconds -> Maybe Number
calcWPM wordsCount (Just(Seconds sec)) = Just( ((toNumber wordsCount) / sec) * 60.0 )
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

text1 :: String
text1 = "The world of Dark Souls is a world of cycles. Kingdoms rise and fall, ages come and go, and even time can end and restart as the flame fades and is renewed. These cycles are linked to the First Flame, a mysterious manifestation of life that divides and defines separate states such as heat and cold, or life and death. As the First Flame fades, these differences also begin to fade, such as life and death having little distinction, and humans becoming Undead. The onset of an Age of Dark, the time when the First Flame has fully died, is marked by endless nights, rampant undeath, time, space, and reality breaking down, lands collapsing and converging on one another, people mutating into monsters, darkness covering the world, and the Gods losing their power. To avoid this and prolong the Age of Fire, the bearer of a powerful soul must 'link' themselves to the First Flame, becoming the fuel for another age. If this is not done, the First Flame will eventually die, and an Age of Dark will begin."

text2 :: String
text2 = "Legendary city of the Witch of Izalith, one of the four original Lords. She and her daughters founded this city deep underground. The Witch and her daughters were originally practitioners of a form of fire sorcery, but this art was lost when the Flame of Chaos destroyed the city. When the First Flame began to fade, the Witch of Izalith tried to reproduce it using her magic and a special soul. However, she failed, and the result was the Flame of Chaos a twisted flame that produced distorted life instead, known as the demons. Gwyn and his knights waged a war against these demons and eventually defeated them, sealing the Bed of Chaos, their source of life, with magic. Some demons now appear in roles that suggest they may work for the gods, either willingly or as captured servants. The creation of the Flame of Chaos also created pyromancy as a byproduct, and Quelana, the only Daughter of Izalith to escape the destruction, was able to develop an entire system of magic to imitate the ancient fire spells. She took only one student, Salaman from the Great Swamp. In teaching Salaman, she hoped that mankind could learn to harness and control the flames, to prevent the same catastrophy that destroyed her own home. Salaman then returned to the Great Swamp to spread the art of pyromancy."

text3 :: String
text3 = "Bearer of the strongest Lord Soul, Gwyn's power manifested as great spears of sunlight, which take the form of lightning. He founded the kingdom of Anor Londo, lead an army of his silver knights against the dragons, and was father to Gwynevere, Gwyndolin, and his disowned firstborn son thought to be Sen, the ancient God of War. Gwyn was the first to link the First Flame, becoming a Lord of Cinder, and now mindlessly guards it against outside threats and serves as a test of strength for those seeking to link it again. Gwyn's crown is said to have once had some special power, but its power has long since faded and now only exudes a slight warmth by the time the player acquires it. Gwyn and his clan are referred to as gods. Whether this is by Gwyn's own command or simply a title given to them by others for their strength is unknown, though they are still the primary religious figures in many human kingdoms. Gwyn is one of the four who happened upon the Lord Souls at the dawn of the Age Of Fire, along with Gravelord Nito, The Witch of Izalith and The Furtive Pygmy."

text4 :: String
text4 = "Youngest son of Gwyn, and the only god who stayed behind in Anor Londo. Gwyndolin has a deep adoration of the sun, and reveres his father, spending his time guarding Gwyn's tomb and orchestrating events alongside Kingseeker Frampt to delay the Age of Dark. Due to his strong connection to the moon and skill in its magic, he was raised as a daughter. Gwyndolin specializes in a form of sorcery that uses Faith instead of Intelligence, and he is capable of powerful illusions that are nearly indistinguishable from reality. In this way, he maintains the appearance of a thriving Anor Londo. In reality, the kingdom has long since been abandoned and experiences an endless night, and Gwyndolin and his Darkmoon Knights are the only ones who remain. Undead may join his company of knights if they wish, though any who dispell the illusion of Gwynevere or enter Gwyn's tomb are instead marked as irredeemable sinners and pursued by those same knights."

text5 :: String
text5 = "One of the four original Lord Soul bearers, her Lord Soul granted the power of fire. She is a witch and master of the now forgotten fire sorceries, and founded the city of Izalith with her daughters deep underground. When the First Flame began to fade, she attempted to create a second First Flame to replace it using her witchcraft and a special soul, but failed catastrophically and instead created the Flame of Chaos. The birth of Chaos marked the birth of demons, and its flame transformed the Witch of Izalith and all her daughters into demons barring Quelana, who escaped the destruction of her home. As the daughters of Izalith were the only practitioners of fire sorcery, it is now a forgotten art, but the creation of Chaos also created pyromancy as a byproduct. Gwyn waged war against the demons and eventually routed them, shackling the Bed of Chaos, their source of life, to contain them."



myWords :: Int->Array String
myWords int  = split (Pattern " ") (pickLore int)

lore :: Array String
lore = [text1,text2,text3,text4,text5]

pickLore :: Int -> String
pickLore position = fromJustString(index lore position)

generateRandomNumber :: Effect Int
generateRandomNumber = randomInt 100000 999999


getStringID::String->String                 
getStringID id = id


repeatAction :: Emitter Aff Action -> Number -> Action -> Aff Unit
repeatAction emitter t action = aux
  where
  aux = do
    Aff.delay (Milliseconds t)
    ES.emit emitter action
    aux

-- message types:
type EnemyState = { playerID::String, player2WPM :: Maybe Number, correctWords :: Int }
type ID = { playerID::String, name :: String, textNumber::Int }

messageToAction :: String -> Either String ActionPVP
messageToAction msg = do
  json <- (parseJson msg) # (describeErr "Failed to parse message as JSON: ")
  (parseSetPlayer json <|||> parseSetIt json) # describeErrs "Failed to decode JSON:\n"
  where
  parseSetPlayer json = do
    ({playerID,player2WPM, correctWords} :: EnemyState) <- decodeJson json
    pure (UpdatePlayer2 player2WPM  correctWords playerID)
  parseSetIt json = do
    ({playerID,name,textNumber} :: ID) <- decodeJson json
    pure (SetPlayerConnected playerID name textNumber )

  describeErr :: forall b.String -> Either JsonDecodeError b -> Either String b
  describeErr s = mapLeft (\ err -> s <> (printJsonDecodeError err))
  describeErrs :: forall b.String -> Either (Array JsonDecodeError) b -> Either String b
  describeErrs s = mapLeft (\ errs -> s <> String.joinWith "\n" (map printJsonDecodeError errs))

