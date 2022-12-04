{-# LANGUAGE OverloadedStrings #-}
module UI where

import GameComponent
import GameView
import GameController
import Paths_CSE230_Haskell_Sokoban

import Brick
  ( App(..), BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  )
import Brick.BChan
import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)

import qualified Graphics.Vty as V

data Tick = Tick


-- register draw handle
app :: App World Tick Name
app = App { appDraw = drawWorld
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

runMyApplication :: IO()
runMyApplication = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay $ tickTimeMS * 1000 -- decides how fast world moves, micro-second

  -- create world
  w <- readFilCreateWorld

  -- create vty
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder

  void $ customMain initialVty builder (Just chan) app w

readFilCreateWorld :: IO World
readFilCreateWorld = do
  -- load init data, create world
  realFileName <- getDataFileName "gameData.txt"
  initDataStr <- readFile realFileName
  let legalData = map addBorderProperly $ rInitData initDataStr
  return $ initialWorld legalData

-- Handling events

-- manipulate man 1
handleEvent :: World -> BrickEvent Name Tick -> EventM Name (Next World)
handleEvent w (AppEvent Tick) = continue $ processTick w

handleEvent w (VtyEvent (V.EvKey (V.KChar 'w') []))
  | state w == GameRunning = continue $ processManTryMove DirectUp w
  | otherwise = continue w
 
handleEvent w (VtyEvent (V.EvKey (V.KChar 'a') []))
  | state w == GameRunning = continue $ processManTryMove DirectLeft w
  | otherwise = continue w

handleEvent w (VtyEvent (V.EvKey (V.KChar 's') [])) 
  | state w == GameRunning = continue $ processManTryMove DirectDown w
  | otherwise = continue w

handleEvent w (VtyEvent (V.EvKey (V.KChar 'd') []))
  | state w == GameRunning = continue $ processManTryMove DirectRight w
  | otherwise = continue w

-- manipulate man 2
-- disable keys if playerNum is 1
handleEvent w (VtyEvent (V.EvKey (V.KUp) []))
  | currentPlayerNum w == "1" = continue w
  | state w == GameRunning = continue $ processMan2TryMove DirectUp w
  | otherwise = continue w
 
handleEvent w (VtyEvent (V.EvKey (V.KLeft) []))
  | currentPlayerNum w == "1" = continue w
  | state w == GameRunning = continue $ processMan2TryMove DirectLeft w
  | otherwise = continue w

handleEvent w (VtyEvent (V.EvKey (V.KDown) []))
  | currentPlayerNum w == "1" = continue w
  | state w == GameRunning = continue $ processMan2TryMove DirectDown w
  | otherwise = continue w

handleEvent w (VtyEvent (V.EvKey (V.KRight) []))
  | currentPlayerNum w == "1" = continue w
  | state w == GameRunning = continue $ processMan2TryMove DirectRight w
  | otherwise = continue w

--  go through different stage
handleEvent w (VtyEvent (V.EvKey (V.KChar 'j') []))
  | state w == GameSelecting = 
        if cix > 0 then continue $ changeToIndexWorld (cix - 1) w else continue w
  | otherwise = continue w
  where 
    cix = currentInitGameDataIx w

handleEvent w (VtyEvent (V.EvKey (V.KChar 'k') []))
  | state w == GameSelecting = 
        if cix < (dl - 1) then continue $ changeToIndexWorld (cix + 1) w else continue w
  | otherwise = continue w
  where 
    dl = length (allInitGameData w)
    cix = currentInitGameDataIx w

handleEvent w (VtyEvent (V.EvKey V.KEnter []))
  | state w == GameReady = continue $ changeToIndexWorld 0 w
  | state w == GameFinished || state w == GameAborted || state w == GameSelecting = 
      continue $ restartWorld w
  | otherwise = continue w

handleEvent w (VtyEvent (V.EvKey (V.KChar 'q') []))
  | state w == GameReady = halt w
  | state w == GameRunning = continue $ w {state = GameAborted}
  | state w == GameFinished || state w == GameAborted = continue 
                $ changeToIndexWorld (currentInitGameDataIx w) w
  | otherwise = continue $ w{state = GameReady}
-- handleEvent w (VtyEvent (V.EvKey V.KEsc []))        = halt w NO ESC NOW
handleEvent w _                                     = continue w