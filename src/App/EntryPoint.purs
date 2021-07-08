
module App.EntryPoint where

import Prelude

import CSS (StyleM, color)
import CSS.Color (red, green)
import Control.Apply (lift2)
import Control.Monad.State (class MonadState)
import Data.Argonaut (JsonDecodeError, decodeJson, encodeJson, parseJson, printJsonDecodeError, stringify)
import Data.Argonaut as Console
import Data.Array (index, (!!))
import Data.Array.NonEmpty (findLastIndex)
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
import SupJS (startGame, changeParticleSpeed, changeParticleSpeedandWidth, cleanInputBox, disableInputBox, fixPVPmagic, fixPVPmagicPositioning, renderMagicJs, renderMagicJsPVP, resizeMagic, changeParticleSpeed2)
import WSListener (setupWSListener)
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WS
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.HTML.Window (document) as Web
import Web.HTML (window) as Web
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.HTML.HTMLDocument as HTMLDocument
import Web.UIEvent.KeyboardEvent as KE

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
              difficulty::String,
              easyColorButton::String,
              mediumColorButton::String,
              hardColorButton::String,
              combatOutcome::String,
              zombiePace::Number,
              penalty::Number,
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
              textNo::Int,
              showText::Boolean}
              

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
    difficulty:"medium",
    zombiePace:8.0,
    penalty:4.0,
    easyColorButton:"grey",
    mediumColorButton:"orange",
    hardColorButton:"grey",
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
              textNo:textNumber,
              showText:false}


data Action = ActionEntry ActionEntry | ActionPVE ActionPVE |ActionPVP ActionPVP 


data ActionEntry = RunPVE | RunPVP String  | SetName String --(| HandleSpacebar H.SubscriptionId KeyboardEvent
data ActionPVE =  RunEntry | Update | SendInput String | Decrement SubscriptionId | OnEasyButtonClick | OnMediumButtonClick | OnHardButtonClick
data ActionPVP=  RunEntrypvp | Updatepvp SubscriptionId | SendInputpvp String |DecrementInitialTimer SubscriptionId | Decrementpvp SubscriptionId | ReceiveMessage String | UpdatePlayer2 (Maybe Number) Int String | SetPlayerConnected String String Int 


entryComponent :: forall t1535 t1536 t1558 t1561. MonadEffect t1535 => MonadAff t1535 => Component HTML t1561 t1558 t1536 t1535
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
       -- HandleSpacebar sid ev -> pure unit
        RunPVE -> do                
         random <- liftEffect $ randomInt 0 4
         let textNumber = fromJustInt((parseInt (show random) (toRadix 10)))
         H.put (PVE (initialPVEstate (textNumber)))
         pure unit   
         --subscribe to keyboard to track spacebar
         --document <- liftEffect $ Web.document =<< Web.window
         --H.subscribe' \sid ->
                    --ES.eventListenerEventSource
                    --KET.keydown
                    --(HTMLDocument.toEventTarget document)
                    --(map (HandleSpacebar sid) <<< KE.fromEvent)
        --fix magic particles
         _<-liftEffect $ renderMagicJs unit
         pure unit 
          
        RunPVP name -> do
            ws <- liftEffect $ WS.create "wss://typing-mage.herokuapp.com" []
            liftAff $ delay (Milliseconds 1000.0) -- allow ws to initialise
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
                                                       let updatedParticlesWidth = pvpState.particlesWidth + (calculateMagicPush previousEnemyCorrectWords numberOfCorrectWords)
                                                       log(show updatedParticlesWidth)
                                                       H.modify_ $ updatePVP $ \st->st{particlesWidth=updatedParticlesWidth,enemyWPM=wpm,enemyCorrectWords=numberOfCorrectWords}
                                                       void $ liftEffect $ changeParticleSpeed2 (fromJustNumber pvpState.enemyWPM)
                                                      else pure unit
                                                     _->do
                                                       pure unit
                                                  pure unit
       SetPlayerConnected id opponentName textId  -> do
                                    state <- H.get
                                    case state of
                                     PVP pvpState ->do
                                      if(pvpState.myID /= id && pvpState.hasOpponent==false)
                                      then
                                       do
                                       H.modify_ $ updatePVP $ \st->st{enemyName=opponentName,opponentID=Just id, hasOpponent=true,showText=true}
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
                let myNewZombiePosition = pveState.zombiePosition+zombiePushValue s myText pveState.penalty
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
                let newMagic=(resizeMagic (pveState.zombiePosition))
                let newtimer = pveState.timer - 1       
                if (pveState.timer>0) && (pveState.zombiePosition>(-50.0))
                then
                do
                    let newZombiePosition = pveState.zombiePosition-pveState.zombiePace
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
        OnEasyButtonClick -> H.modify_ $ updatePVE \st-> st{ difficulty="easy",easyColorButton = "green",mediumColorButton="grey",hardColorButton="grey",zombiePace=7.0,penalty=3.0}
        OnMediumButtonClick -> H.modify_ $ updatePVE \st-> st{ difficulty="medium",easyColorButton = "grey",mediumColorButton="orange",hardColorButton="grey",zombiePace=8.0,penalty=4.0}
        OnHardButtonClick -> H.modify_ $ updatePVE \st-> st{ difficulty="hard",easyColorButton = "grey",mediumColorButton="grey",hardColorButton="darkred",zombiePace=10.0,penalty=5.0}
                 

    render (Entry entrystate) =
     HH.div
        [style "width:80%; background-color:#2c2f33; display:flex;flex-wrap:wrap; justify-content:center;allign-items:center;"]
        [HH.div
        [style flexDivCenterItems]
        [HH.button  
        [style (buttonsCss "1392A4" "0AD4F0"), HE.onClick \_ -> Just (ActionEntry RunPVE) ]
        [ HH.text "Play PVE" ] 
        ],
        HH.div
        [style flexDivCenterItems]
        [HH.button 
        [ style (buttonsCss "BC6513" "E48D3A"), HE.onClick \_ -> Just (ActionEntry (RunPVP $ fromJustString entrystate.name)) ]
        [ HH.text "Play PVP" ]
            ],
        HH.div
        [style flexDivCenterItems]
        [HH.input [HP.placeholder "What is your name?" ,style inputBoxCSS, HE.onValueInput \s -> Just(ActionEntry(SetName s)) ]
            ],
        HH.div
        [style flexDivCenterItems]
        [HH.p
        [style"color:yellow;font:40px Comic Sans;min-width:300px;text-align:center;"] 
            [ HH.text $ "Wizard name: " <> fromJustString entrystate.name]]
        ,HH.div
        [style flexDivCenterItems]
        [HH.img
        [HP.src  "images/grimoire.png"
        ,HP.height 400
        ,HP.width 600]]
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
        [style"display:inline-flex;width:100%;justify-self:center;margin-top:10%;margin-bottom:15%;background-color:transparent;"]
        [HH.div
        [style("position:absolute;display:flex; flex-wrap:wrap;justify-content:center;justify-self:center;left:15%;top:32%")]
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
            [HH.text $  (myPVPstate.playerName)]
        ,HH.p
        [style"position:relative;font: 40px Tahoma, Helvetica, Arial, Sans-Serif;text-align: center;color:yellow;text-shadow: 0px 2px 3px #555;"] 
            [HH.text $  (myPVPstate.enemyName)]
        ]
        ]
        ,
        HH.div
        [style "width:100%; background-color:#2c2f33;display:flex; flex-wrap:wrap;justify-content:center;justify-self:center;margin-top:10%"]
        [HH.div
        [style "justify-self:center;width:90%;background-color:transparent;display:flex;justify-content:center;flex-wrap:wrap;"]
        [HH.p
        [style"font: 40px Tahoma, Helvetica, Arial, Sans-Serif;text-align: right; color:white; text-shadow: 0 0 7px #fff, 0 0 10px #fff, 0 0 21px #fff, 0 0 42px #0fa, 0 0 82px #0fa, 0 0 92px #0fa, 0 0 102px #0fa, 0 0 151px #0fa;"] 
            [HH.text $  (fromJustString ((myWords myPVPstate.textNo) !! myPVPstate.wordCounter))]
        ,HH.p
        [style"margin-left:3%;font: 40px Tahoma, Helvetica, Arial, Sans-Serif;text-align: center;color:orange;text-shadow: 0px 2px 3px #555;width:wrap-content;"] 
            [HH.text $  (fromJustString ((myWords myPVPstate.textNo) !! (myPVPstate.wordCounter+1)))<>" "<> (fromJustString ((myWords myPVPstate.textNo) !! (myPVPstate.wordCounter+2)))]
        ]
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
     [style "width:50%; background-color:#2c2f33;"][
        HH.div
     [style "display:flex;justify-content:center;position:absolute;width:inherit;"][
       HH.div
        [style ""]
        [HH.button 
        [ style (difficultyBtnCSS myPVEstate.easyColorButton "white"),HE.onClick \_ -> Just (ActionPVE OnEasyButtonClick) ]
        [ HH.text "Easy" ]
            ],
        HH.div
        [style ""]
        [HH.button 
        [ style (difficultyBtnCSS myPVEstate.mediumColorButton "white"),HE.onClick \_ -> Just (ActionPVE OnMediumButtonClick) ]
        [ HH.text "Medium" ]
            ],
        HH.div
        [style ""]
        [HH.button 
        [ style (difficultyBtnCSS myPVEstate.hardColorButton "white"),HE.onClick \_ -> Just (ActionPVE OnHardButtonClick) ]
        [ HH.text "Hard" ]
            ]],
         HH.div
        [style $ modalCSS myPVEstate.showGameEndModal][
            HH.p
            [style $ pveOutcome myPVEstate.combatOutcome] 
            [ HH.text myPVEstate.combatOutcome],
            HH.div[style"position:relative;width:100%;height:100px;"][
            HH.p
            [style "width:100%;font-size:30px;color:cyan;text-shadow: 0px 0px 5px #555;margin:10px;text-align:center;"] 
            [ HH.text $ "WPM: " <> toStringWith (precision 3 ) (fromJustNumber myPVEstate.wpm)]
            ],
            HH.div[style"position:relative;width:100%;height:100px;"][
            HH.p
            [style "width:100%;font-size:30px;color:cyan;text-shadow: 0px 0px 5px #555;margin:10px;text-align:center;"] 
            [ HH.text $ "Correct Words: " <> show (myPVEstate.wordCounter-myPVEstate.wrongWordCounter)]
            ],
            HH.div[style"position:relative;width:100%;height:100px;"][
            HH.p
            [style "width:100%;font-size:20px;color:orange;text-shadow: 0px 0px 5px #555;margin:10px;text-align:center;"] 
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
        [HH.div
        [style "justify-self:center;width:90%;background-color:transparent;display:flex;justify-content:center;flex-wrap:wrap;"]
        [HH.p
        [style"font: 40px Tahoma, Helvetica, Arial, Sans-Serif;text-align: right; color:white; text-shadow: 0 0 7px #fff, 0 0 10px #fff, 0 0 21px #fff, 0 0 42px #0fa, 0 0 82px #0fa, 0 0 92px #0fa, 0 0 102px #0fa, 0 0 151px #0fa;"] 
            [HH.text $  (fromJustString ((myWords myPVEstate.textNo) !! myPVEstate.wordCounter))]
        ,HH.p
        [style"margin-left:3%;font: 40px Tahoma, Helvetica, Arial, Sans-Serif;text-align: center;color:orange;text-shadow: 0px 2px 3px #555;width:wrap-content;"] 
            [HH.text $  (fromJustString ((myWords myPVEstate.textNo) !! (myPVEstate.wordCounter+1)))<>" "<> (fromJustString ((myWords myPVEstate.textNo) !! (myPVEstate.wordCounter+2)))]
        ],HH.input
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
 | flag == true = "width:50%; height:wrap-content;left:25%;top:15%;z-index:999; position:absolute;background-color:#23272a;display:block;justify-content:space-between;overflow:wrap;border-width:5px;border-style:groove;border-color:orange;"
 | otherwise = "width:50%; height:wrap-content;left:25%;top:15%;z-index:999; position:absolute;background-color:#23272a;display:none;border-width:5px;border-style:groove;border-color:orange;"

battleOutcomeTitle :: String -> String
battleOutcomeTitle outcome 
 | outcome == "Victory" = "width:100%;margin: 50px auto;text-align: center;text-shadow: -1px -1px 0px rgba(255,255,255,0.3), 1px 1px 0px rgba(0,0,0,0.8);color:rgba(30, 130, 76, 1);opacity: 1;font: 700 80px 'Bitter'"
 | outcome == "Defeat" = "width:100%;margin: 50px auto;text-align: center;text-shadow: -1px -1px 0px rgba(255,255,255,0.3), 1px 1px 0px rgba(0,0,0,0.8);color:rgba(150, 40, 27, 1);opacity: 1;font: 700 80px 'Bitter'"
 | otherwise =":width:100%;margin: 50px auto;text-align: center;text-shadow: -1px -1px 0px rgba(255,255,255,0.3), 1px 1px 0px rgba(0,0,0,0.8);color:rgba(75, 119, 190, 1);opacity: 1;font: 700 80px 'Bitter'"

pveOutcome :: String -> String
pveOutcome outcome 
 | outcome == "Win" = "width:100%;margin: 50px auto;text-align: center;text-shadow: -1px -1px 0px rgba(255,255,255,0.3), 1px 1px 0px rgba(0,0,0,0.8);color:rgba(30, 130, 76, 1);opacity: 1;font: 700 80px 'Bitter'"
 | otherwise = "width:100%;margin: 50px auto;text-align: center;text-shadow: -1px -1px 0px rgba(255,255,255,0.3), 1px 1px 0px rgba(0,0,0,0.8);color:rgba(150, 40, 27, 1);opacity: 1;font: 700 80px 'Bitter'"

difficultyBtnCSS ::String ->String-> String
difficultyBtnCSS color colorText = "outline:0;"<>
  "background:#fff;"<>
  "border:none;"<>
  "padding:2em 4em;"<>
  "background-color:"<>color<>";"<>
  "color:"<>colorText<>";"<>
  "width:50px"<>
  "height:100px;"




buttonsCss :: String->String-> String
buttonsCss color1 color2 = "width:200px;height:70px;"<>
    "background: linear-gradient(to left top, #"<>color1<>" 50%, #"<>color2<>" 50%);"<>
    "border-style: none;"<>
    "color:#fff;"<>
    "font-size: 23px;"<>
    "letter-spacing: 3px;"<>
    "font-family: 'Lato';"<>
    "font-weight: 600;"<>
    "outline: none;"<>
    "position: relative;"<>
    "margin: 10px;"<>
    "overflow: hidden;"<>
    "box-shadow: 0px 1px 2px rgba(0,0,0,.2);"

flexDivCenterItems :: String
flexDivCenterItems="width:100%; display:flex; flex-wrap:wrap;justify-content:center; background-color:transparent;"

makeTextVisible :: Boolean -> String
makeTextVisible flag 
 | flag == true = "font: 40px Tahoma, Helvetica, Arial, Sans-Serif;text-align: center;color:orange;text-shadow: 0px 2px 3px #555;min-width:100%;display:block;"
 | otherwise = "font: 40px Tahoma, Helvetica, Arial, Sans-Serif;text-align: center;color:orange;text-shadow: 0px 2px 3px #555;min-width:100%;display:none;"

inputBoxCSS :: String
inputBoxCSS="border: none;border-bottom: 2px solid orange;background:transparent; width:60%;height:60px;font-size:50px;color:white;text-align:center;margin:10px;"
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

zombiePushValue :: forall t5. Eq t5 => t5 -> t5 -> Number -> Number
zombiePushValue input word penalty
    | input == word = 2.0
    | otherwise = -penalty
magicPushCalculator :: forall t8. Eq t8 => t8 -> t8 -> Number
magicPushCalculator input word
    | input == word = 10.0
    | otherwise = 0.0

text1 :: String
text1= "He walked down the steps from the train station in a bit of a hurry knowing the secrets in the briefcase must be secured as quickly as possible. Bounding down the steps, he heard something behind him and quickly turned in a panic. There was nobody there but a pair of old worn-out shoes were placed neatly on the steps he had just come down. Had he past them without seeing them? It didn't seem possible. He was about to turn and be on his way when a deep chill filled his body. Colors bounced around in her head. They mixed and threaded themselves together. Even colors that had no business being together. They were all one, yet distinctly separate at the same time. How was she going to explain this to the others? What were they eating? It didn't taste like anything she had ever eaten before and although she was famished, she didn't dare ask. She knew the answer would be one she didn't want to hear."
text2 :: String
text2="It was a rat's nest. Not a literal one, but that is what her hair seemed to resemble every morning when she got up. It was going to take at least an hour to get it under control and she was sick and tired of it. She peered into the mirror and wondered if it was worth it. It wasn't. She opened the drawer and picked up the hair clippers. The alarm went off and Jake rose awake. Rising early had become a daily ritual, one that he could not fully explain. From the outside, it was a wonder that he was able to get up so early each morning for someone who had absolutely no plans to be productive during the entire day. The trees, therefore, must be such old and primitive techniques that they thought nothing of them, deeming them so inconsequential that even savages like us would know of them and not be suspicious. At that, they probably didn't have too much time after they detected us orbiting and intending to land. And if that were true, there could be only one place where their civilization was hidden."
text3 :: String
text3 = "Sitting in the sun, away from everyone who had done him harm in the past, he quietly listened to those who roamed by. He felt at peace in the moment, hoping it would last, but knowing the reprieve would soon come to an end. He closed his eyes, the sun beating down on face and he smiled. He smiled for the first time in as long as he could remember. Are you getting my texts??? she texted to him. He glanced at it and chuckled under his breath. Of course he was getting them, but if he wasn't getting them, how would he ever be able to answer? He put the phone down and continued on his project. He was ignoring her texts and he planned to continue to do so. The boy walked down the street in a carefree way, playing without notice of what was about him. He didn't hear the sound of the car as his ball careened into the road. He took a step toward it, and in doing so sealed his fate."
text4 :: String
text4 = "She never liked cleaning the sink. It was beyond her comprehension how it got so dirty so quickly. It seemed that she was forced to clean it every other day. Even when she was extra careful to keep things clean and orderly, it still ended up looking like a mess in a couple of days. What she didn't know was there was a tiny creature living in it that didn't like things neat. He sat across from her trying to imagine it was the first time. It wasn't. Had it been a hundred? It quite possibly could have been. Two hundred? Probably not. His mind wandered until he caught himself and again tried to imagine it was the first time. Green vines attached to the trunk of the tree had wound themselves toward the top of the canopy. Ants used the vine as their private highway, avoiding all the creases and crags of the bark, to freely move at top speed from top to bottom or bottom to top depending on their current chore. At least this was the way it was supposed to be. Something had damaged the vine overnight halfway up the tree leaving a gap in the once pristine ant highway."
text5 :: String
text5 = "Her eyebrows were a shade darker than her hair. They were thick and almost horizontal, emphasizing the depth of her eyes. She was rather handsome than beautiful. Her face was captivating by reason of a certain frankness of expression and a contradictory subtle play of features. Her manner was engaging. It was difficult for him to admit he was wrong. He had been so certain that he was correct and the deeply held belief could never be shaken. Yet the proof that he had been incorrect stood right before his eyes. See daddy, I told you that they are real! his daughter excitedly proclaimed. It went through such rapid contortions that the little bear was forced to change his hold on it so many times he became confused in the darkness, and could not, for the life of him, tell whether he held the sheep right side up, or upside down. But that point was decided for him a moment later by the animal itself, who, with a sudden twist, jabbed its horns so hard into his lowest ribs that he gave a grunt of anger and disgust."

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

