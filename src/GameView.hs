{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module GameView where

import GameController
import GameComponent

import Data.List (find)
import Data.Maybe(fromJust)
import Brick
  ( AttrMap, Widget, hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, padLeftRight, padTopBottom, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  , (<=>)
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

type Name = ()

-- Drawing
drawWorld :: World -> [Widget Name]
drawWorld w = case state w of
  GameReady -> drawWelcome w
  _ ->[ C.center $ infoAreaW <+> playAreaW <=> instrAreaW]
      where infoAreaW = drawStats w
            playAreaW = padLeft (Pad 4) $ padTop (Pad 4) $ drawGrid w
            instrAreaW = padTop (Pad 2) $ drawIns w

drawIns :: World -> Widget Name
drawIns w = instruction
  where instruction
          | state w == GameAborted = str "play again <Enter> | back to select <q>"
          | state w == GameFinished = str "play again <Enter> | back to select <q>"
          | state w == GameSelecting = str "start <Enter> | previous <j> | next <k> | return <q>"
          | state w == GameRunning && currentPlayerNum w == "1" = str "Move the character by \"WASD\" | abort game <q>"
          | state w == GameRunning && currentPlayerNum w == "2" = str "Move character 1 by \"WSAD\", character 2 by \"▲▼◀▶\" | abort game <q>"

drawWelcome :: World -> [Widget Name]
drawWelcome w = [ C.center $ vBox [C.hCenter welcomePaint, padTop (Pad 3) (welcomeText1 <=> welcomeText2)] ]
  where 
    welcomeText1 = C.hCenter $ hLimit (34 * 2) $ str "Welcome to Sokoban!"
    welcomeText2 = C.hCenter $ hLimit (34 * 2) $ str "Press <Enter> for new game | <q> to exit."
    welcomePaint = hBox (map dummyDraw "s o k o b a n" )

drawStats :: World -> Widget Name
drawStats w = hLimit 25 $ vBox usingStats
  where usingStats
          | state w == GameSelecting = normalStats
          | otherwise  = normalStats ++ runningStats
        normalStats = [ padAll (3) $ drawSlogan (state w) (runningTicks w), drawLevel $ currentLevel w,  drawPlayerNum $ currentPlayerNum w]
        runningStats = [drawSteps ((steps $ head (man w)) + (steps $ last (man w))), drawTime $ runningTicks w]

-- -- helper
-- drawManPosition :: Point -> Widget Name
-- drawManPosition p = withBorderStyle BS.unicodeBold
--   $ B.borderWithLabel (str "pos")
--   $ C.hCenter
--   --  $ padTopBottom 1
--   $ padLeftRight 2
--   $ str $ show p

drawLevel :: String -> Widget Name
drawLevel l = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str "Level")
  $ C.hCenter
  $ padLeftRight 2
  $ paintOnLevel l
  where paintOnLevel l = case l of
          "Medium" -> withAttr mediumLvAttr $ str l
          "Easy" -> withAttr easyLvAttr $ str l
          _ -> withAttr hardLvAttr $ str l

drawPlayerNum :: String -> Widget Name
drawPlayerNum l = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str "Player Number")
  $ C.hCenter
  $ padLeftRight 2
  $ paintOnLevel l
  where paintOnLevel l = case l of
          "1" -> withAttr easyLvAttr $ str l
          _ -> withAttr mediumLvAttr $ str l

drawSteps :: Int -> Widget Name
drawSteps n = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str "Steps")
  $ C.hCenter
  $ padLeftRight 2
  $ str $ show n

drawTime :: Int -> Widget Name
drawTime n = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str "Time (s)")
  $ C.hCenter
  $ padLeftRight 2
  $ str $ show (n * tickTimeMS `div` 1000)

drawSlogan ::  GameState -> Int -> Widget Name
drawSlogan st tk
  | st == GameAborted = withAttr gameAbortedAttr $ C.hCenter $ str "GAME ABORT"
  | st == GameFinished = withAttr gameFinishedAttr $ C.hCenter $ str "GAME SUCCESS"
  | st == GameSelecting = withAttr gameSelectingAttr $ C.hCenter $ str
        $ if (tk * tickTimeMS `div` 1000 `mod` 2) == 0 then "SELECTING..." else "        "
  | st == GameRunning = withAttr gameRunningAttr $ C.hCenter $ str 
        $ if (tk * tickTimeMS `div` 500 `mod` 2) == 0 then "FIGHTING!" else "        "
  | otherwise = emptyWidget
    -- shining
    


drawGrid :: World -> Widget Name
drawGrid w = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str $ "Stage: " ++ showN )
  $ vBox rows
  where
    showN = if stageN < 10 then "0" ++ show stageN else show stageN
    stageN = currentInitGameDataIx w
    rows         = [hBox $ cellsInRow r | r <- reverse [(- halfH)..halfH]]
    cellsInRow y = [drawPoint (x, y) | x <- [(-halfW)..halfW]]
    drawPoint    =  drawPointFromWorld w
    halfW = areaHalfWidth w
    halfH = areaHalfHeight w

drawPointFromWorld :: World -> Point -> Widget Name
drawPointFromWorld  w p
  | p == man1Po = drawMan $ head (man w)
  | currentPlayerNum w == "2" && p == man2Po = drawMan $ last (man w)
  | p `elem` boxPos && p `elem` holePos = drawBox True $ fromJust $ find (\(Box pb) -> pb == p) bs
  | p `elem` boxPos = drawBox False $ fromJust $ find (\(Box pb) -> pb == p) bs
  | p `elem` holePos = drawHole $ fromJust $ find (\(Hole ph) -> ph == p) hs
  | p `elem` wallBrickPos = drawWallBrick $ fromJust $ find (\(WallBrick pw) -> pw == p) ws
  | otherwise = drawEmpty
    where
      man1Po = positionOfMan $ head (man w)
      man2Po = positionOfMan $ last (man w)
      wallBrickPos = map positionOfWallBrick ws
      holePos = map positionOfHole hs
      boxPos = map positionOfBox bs
      ws = wallBricks w
      hs = holes w
      bs = boxes w

drawMan :: Man -> Widget Name
drawMan man = withAttr manAttr $ case direct man of
  DirectUp ->  manUpSquare
  DirectDown -> manDownSquare
  DirectLeft -> manLeftSquare
  DirectRight -> manRightSquare

drawWallBrick :: WallBrick -> Widget Name
drawWallBrick wallBrick = withAttr wallBrickAttr wallBrickSquare

drawHole :: Hole -> Widget Name
drawHole hole = withAttr holeAttr holeBigSquare

drawBox :: Bool -> Box  -> Widget Name
drawBox ok box = if ok then withAttr okBoxAttr $ boxBigSquare else withAttr boxAttr $ boxBigSquare

drawEmpty :: Widget Name
drawEmpty = withAttr emptyAttr bigSquare

oneS :: Widget Name
oneS = str "  "

twoSH :: Widget Name
twoSH = hBox [oneS, oneS]

bigSquare :: Widget Name
bigSquare = vBox [twoSH, twoSH]

boxBigSquare :: Widget Name 
boxBigSquare = holeBigSquare

holeBigSquare :: Widget Name 
holeBigSquare = vBox [str " \\/ ", str " /\\ "]

manUpSquare :: Widget Name
manUpSquare = vBox [str " /\\ ", str " || "]

manDownSquare :: Widget Name
manDownSquare = vBox [str " || ", str " \\/ "]

manLeftSquare :: Widget Name
manLeftSquare = vBox [str "/---", str "\\---"]

manRightSquare :: Widget Name
manRightSquare = vBox [str "---\\", str "---/"]

wallBrickSquare :: Widget Name
wallBrickSquare = bigSquare

dummyDraw :: Char -> Widget Name
dummyDraw c = case c of
  's' -> dummyProcess [[0,1,1,1],[1,0,0,0],[0,1,1,0],[0,0,0,1],[1,1,1,0]]
  'o' -> dummyProcess [[1,1,1,1],[1,0,0,1],[1,0,0,1],[1,0,0,1],[1,1,1,1]]
  'k' -> dummyProcess [[1,0,0,1],[1,0,1,0],[1,1,0,0],[1,0,1,0],[1,0,0,1]]
  'b' -> dummyProcess [[1,0,0,0],[1,0,0,0],[1,1,1,1],[1,0,0,1],[1,1,1,1]]
  'a' -> dummyProcess [[1,1,1,1],[1,0,0,1],[1,1,1,1],[1,0,0,1],[1,0,0,1]]
  'n' -> dummyProcess [[1,1,1,1],[1,0,0,1],[1,0,0,1],[1,0,0,1],[1,0,0,1]]
  ' ' -> dummyProcess $ replicate 5 [0]
  _ -> dummyProcess $ replicate 5 [1,1,1,1]


dummyProcess :: [[Int]] -> Widget Name
dummyProcess grid = vBox $ map f grid
  where f arr = hBox $ map (\v -> if v == 1 then withAttr welcomeCharAttr oneS else oneS) arr

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (manAttr, V.black `on` manColor)
  , (boxAttr, ( V.white `on` boxColor )`V.withStyle` V.bold)
  , (okBoxAttr, ( clearRedColor `on` okBoxColor )`V.withStyle` V.bold)
  , (holeAttr,holeColor `on` floorColor `V.withStyle` V.bold)
  , (wallBrickAttr, V.black `on` wallBrickColor)
  , (gameAbortedAttr, fg V.red `V.withStyle` V.bold)
  , (gameFinishedAttr, fg V.green `V.withStyle` V.bold)
  , (gameSelectingAttr, fg V.blue `V.withStyle` V.bold)
  , (gameRunningAttr, fg boxColor `V.withStyle` V.bold)
  , (emptyAttr, V.white `on` floorColor)
  , (welcomeCharAttr, V.black `on` welcomeCharColor)
  , (hardLvAttr, fg hardLvColor)
  , (easyLvAttr, fg V.green)
  , (mediumLvAttr, fg mediumLvColor)
  ]

boxColor :: V.Color
boxColor = V.rgbColor 160 82 45

okBoxColor :: V.Color
okBoxColor = V.rgbColor 152 251 152

holeColor :: V.Color
holeColor = V.rgbColor 50 205 50

wallBrickColor :: V.Color
wallBrickColor = V.rgbColor 169 169 169

floorColor :: V.Color
floorColor = V.rgbColor 214 206 158

clearRedColor :: V.Color
clearRedColor = V.rgbColor 0 128 0

welcomeCharColor :: V.Color
welcomeCharColor = V.rgbColor 253 126 125

hardLvColor :: V.Color
hardLvColor = V.rgbColor 227 23 13

mediumLvColor :: V.Color
mediumLvColor = V.rgbColor 255 97 3

manColor :: V.Color
manColor = V.rgbColor 0 112 205


gameFinishedAttr :: AttrName
gameFinishedAttr = "gameFinished"

gameAbortedAttr :: AttrName
gameAbortedAttr = "gameAborted"

gameSelectingAttr :: AttrName
gameSelectingAttr = "gameSelecting"

gameRunningAttr :: AttrName
gameRunningAttr = "gameRunning"

manAttr, 
  boxAttr, okBoxAttr, 
  holeAttr, 
  wallBrickAttr, 
  emptyAttr, 
  welcomeCharAttr,
  hardLvAttr,easyLvAttr,mediumLvAttr :: AttrName
manAttr = "manAttr"
holeAttr = "holeAttr"
boxAttr = "boxAttr"
okBoxAttr = "okBoxAttr"
wallBrickAttr = "wallBrickAttr"
emptyAttr = "emptyAttr"
welcomeCharAttr = "welcomCharAttr"
hardLvAttr = "hardLvAttr"
easyLvAttr = "easyLvAttr"
mediumLvAttr = "mediumLvAttr"

