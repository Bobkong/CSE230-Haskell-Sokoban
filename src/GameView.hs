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
drawIns w = vBox [instruction, padTop (Pad 2) $ examples]
  where instruction
          | state w == GameAborted = str "play again <Enter> | back to select <q>" 
          | state w == GameFinished = str "play again <Enter> | back to select <q>" 
          | state w == GameSelecting = str "start <Enter> | previous <j> | next <k> | return <q>" 
          | state w == GameRunning && currentPlayerNum w == "1" = str "move Leo Messi by \"WASD\" | abort game <q>"
          | state w == GameRunning && currentPlayerNum w == "2" = str "move Leo Messi by \"WSAD\", Cristiano Ronaldo by \"▲▼◀▶\" | abort game <q>"
        examples = drawExample w

drawExample :: World -> Widget Name
drawExample w = hBox (characters ++ otherExamples)
  where characters
          | currentPlayerNum w == "1" = [str "Messi", padLeft (Pad 1) $ withAttr man1Attr manUpSquare]
          | currentPlayerNum w == "2" = [str "Messi", padLeft (Pad 1) $ withAttr man1Attr manUpSquare, padLeft(Pad 3) $ str "CR7", padLeft (Pad 1) $ withAttr man2Attr manUpSquare]
        otherExamples = [padLeft(Pad 3) $ str "BALL", padLeft (Pad 1) $ (withAttr boxAttr $ boxBigSquare), padLeft(Pad 3) $ str "GOAL", padLeft (Pad 1) $ withAttr holeAttr holeBigSquare]

drawWelcome :: World -> [Widget Name]
drawWelcome w = [ C.center $ vBox [C.hCenter welcomePaint, padTop (Pad 3) (welcomeText1 <=> welcomeText2)] ]
  where
    welcomeText1 = C.hCenter $ hLimit (34 * 2) $ str "Welcome to Soccoban!"
    welcomeText2 = C.hCenter $ hLimit (34 * 2) $ str "Press <Enter> for new game | <q> to exit."
    welcomePaint = hBox (map dummyDraw "s o c c o b a n" )

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
  $ B.borderWithLabel (str " Level ")
  $ C.hCenter
  $ padLeftRight 2
  $ paintOnLevel l
  where paintOnLevel l = case l of
          "1" -> withAttr easyLvAttr $ str l
          "2" -> withAttr easyLvAttr $ str l
          "3" -> withAttr mediumLvAttr $ str l
          "4" -> withAttr mediumLvAttr $ str l
          _ -> withAttr hardLvAttr $ str l

drawPlayerNum :: String -> Widget Name
drawPlayerNum l = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " # of Players ")
  $ C.hCenter
  $ padLeftRight 2
  $ paintOnLevel l
  where paintOnLevel l = case l of
          "1" -> withAttr easyLvAttr $ str l
          _ -> withAttr mediumLvAttr $ str l

drawSteps :: Int -> Widget Name
drawSteps n = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " # of Moves ")
  $ C.hCenter
  $ padLeftRight 2
  $ str $ show n

drawTime :: Int -> Widget Name
drawTime n = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " Time (s) ")
  $ C.hCenter
  $ padLeftRight 2
  $ str $ show (n * tickTimeMS `div` 1000)

drawSlogan ::  GameState -> Int -> Widget Name
drawSlogan st tk
  | st == GameAborted = withAttr gameAbortedAttr $ C.hCenter $ str "GAME ABORT"
  | st == GameFinished = withAttr gameFinishedAttr $ C.hCenter $ str "GOOOOOAAALLL!!!"
  | st == GameSelecting = withAttr gameSelectingAttr $ C.hCenter $ str
        $ if (tk * tickTimeMS `div` 1000 `mod` 2) == 0 then "SELECTING..." else "        "
  | st == GameRunning = withAttr gameRunningAttr $ C.hCenter $ str
        $ if (tk * tickTimeMS `div` 500 `mod` 2) == 0 then "LET'S GO!" else "        "
  | otherwise = emptyWidget
    -- shining



drawGrid :: World -> Widget Name
drawGrid w = withBorderStyle BS.unicodeRounded
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- reverse [(- halfH)..halfH]]
    cellsInRow y = [drawPoint (x, y) | x <- [(-halfW)..halfW]]
    drawPoint    =  drawPointFromWorld w
    halfW = areaHalfWidth w
    halfH = areaHalfHeight w

drawPointFromWorld :: World -> Point -> Widget Name
drawPointFromWorld  w p
  | p == man1Po = drawMan1 $ head (man w)
  | currentPlayerNum w == "2" && p == man2Po = drawMan2 $ last (man w)
  | p `elem` boxPos && p `elem` holePos = drawBox True (fromJust (find (\(Box pb) -> pb == p) bs)) p
  | p `elem` boxPos = drawBox False (fromJust (find (\(Box pb) -> pb == p) bs)) p
  | p `elem` holePos = drawHole (fromJust (find (\(Hole ph) -> ph == p) hs)) p
  | p `elem` wallBrickPos = drawWallBrick $ fromJust $ find (\(WallBrick pw) -> pw == p) ws
  | otherwise = drawEmpty p
    where
      man1Po = positionOfMan $ head (man w)
      man2Po = positionOfMan $ last (man w)
      wallBrickPos = map positionOfWallBrick ws
      holePos = map positionOfHole hs
      boxPos = map positionOfBox bs
      ws = wallBricks w
      hs = holes w
      bs = boxes w

drawMan1 :: Man -> Widget Name
drawMan1 man = withAttr man1Attr $ case direct man of
  DirectUp ->  manUpSquare
  DirectDown -> manDownSquare
  DirectLeft -> manLeftSquare
  DirectRight -> manRightSquare

drawMan2 :: Man -> Widget Name
drawMan2 man = withAttr man2Attr $ case direct man of
  DirectUp ->  manUpSquare
  DirectDown -> manDownSquare
  DirectLeft -> manLeftSquare
  DirectRight -> manRightSquare

drawWallBrick :: WallBrick -> Widget Name
drawWallBrick wallBrick = withAttr wallBrickAttr wallBrickSquare

drawHole :: Hole -> Point -> Widget Name
drawHole hole p = withAttr (if (fst p) `rem` 2 == 0 then holeAttr else holeAttr2) holeBigSquare

drawBox :: Bool -> Box -> Point -> Widget Name
drawBox ok box p = if ok then withAttr okBoxAttr $ boxBigSquare
  else withAttr (if (fst p) `rem` 2 == 0 then boxAttr else boxAttr2) $ boxBigSquare

drawEmpty :: Point -> Widget Name
drawEmpty p = withAttr (if (fst p) `rem` 2 == 0 then emptyAttr else emptyAttr2) bigSquare

oneS :: Widget Name
oneS = str "  "

twoSH :: Widget Name
twoSH = hBox [oneS, oneS]

bigSquare :: Widget Name
bigSquare = vBox [twoSH, twoSH]

boxBigSquare :: Widget Name
boxBigSquare = vBox [str "⚽⚽", str "⚽⚽"]

holeBigSquare :: Widget Name
holeBigSquare = vBox [str " GO ", str " AL "]

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
  's' -> dummyProcess [[0,1,1,1],[1,0,0,0],[0,1,1,0],[0,0,0,1],[1,1,1,0]] 1
  'o' -> dummyProcess [[1,1,1,1],[1,0,0,1],[1,0,0,1],[1,0,0,1],[1,1,1,1]] 0
  'c' -> dummyProcess [[1,1,1,1],[1,0,0,0],[1,0,0,0],[1,0,0,0],[1,1,1,1]] 1
  'b' -> dummyProcess [[1,0,0,0],[1,0,0,0],[1,1,1,1],[1,0,0,1],[1,1,1,1]] 1
  'a' -> dummyProcess [[1,1,1,1],[1,0,0,1],[1,1,1,1],[1,0,0,1],[1,0,0,1]] 0
  'n' -> dummyProcess [[1,1,1,1],[1,0,0,1],[1,0,0,1],[1,0,0,1],[1,0,0,1]] 1
  ' ' -> dummyProcess (replicate 5 [0]) 2
  _ -> dummyProcess (replicate 5 [1,1,1,1]) 2


dummyProcess :: [[Int]] -> Int -> Widget Name
dummyProcess grid c = vBox $ map f grid
  where
    f arr = hBox $ map (\v -> if v == 1 then withAttr attr oneS else oneS) arr
    attr  = if c == 1 then welcomeCharAttr else welcomeCharAttr2

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (man1Attr, messiColor `on` man1Color)
  , (man2Attr, cr7Color `on` man2Color)
  , (boxAttr, ( V.white `on` boxColor )`V.withStyle` V.bold)
  , (boxAttr2, ( V.white `on` boxColor2 )`V.withStyle` V.bold)
  , (okBoxAttr, ( clearRedColor `on` okBoxColor )`V.withStyle` V.bold)
  , (holeAttr, holeColor `on` floorColor `V.withStyle` V.bold)
  , (holeAttr2, holeColor `on` floorColor2 `V.withStyle` V.bold)
  , (wallBrickAttr, V.black `on` wallBrickColor)
  , (gameAbortedAttr, fg V.red `V.withStyle` V.bold)
  , (gameFinishedAttr, fg V.green `V.withStyle` V.bold)
  , (gameSelectingAttr, fg V.blue `V.withStyle` V.bold)
  , (gameRunningAttr, fg boxColor `V.withStyle` V.bold)
  , (emptyAttr, V.white `on` floorColor)
  , (emptyAttr2, V.white `on` floorColor2)
  , (welcomeCharAttr, V.black `on` welcomeCharColor)
  , (welcomeCharAttr2, V.black `on` welcomeCharColor2)
  , (hardLvAttr, fg hardLvColor)
  , (easyLvAttr, fg V.green)
  , (mediumLvAttr, fg mediumLvColor)
  ]

boxColor :: V.Color
boxColor = floorColor

boxColor2 :: V.Color
boxColor2 = floorColor2

okBoxColor :: V.Color
okBoxColor = V.rgbColor 152 251 152

holeColor :: V.Color
holeColor = V.rgbColor 242 234 70

wallBrickColor :: V.Color
wallBrickColor = V.rgbColor 169 169 169

floorColor :: V.Color
floorColor = V.rgbColor 116 212 52

floorColor2 :: V.Color
floorColor2 = V.rgbColor 16 161 54

clearRedColor :: V.Color
clearRedColor = V.rgbColor 0 128 0

welcomeCharColor :: V.Color
welcomeCharColor = floorColor

welcomeCharColor2 :: V.Color
welcomeCharColor2 = floorColor2 

hardLvColor :: V.Color
hardLvColor = V.rgbColor 227 23 13

mediumLvColor :: V.Color
mediumLvColor = V.rgbColor 255 97 3

man1Color :: V.Color
man1Color = V.rgbColor 108 172 228

messiColor :: V.Color
messiColor = V.white

man2Color :: V.Color
man2Color = V.rgbColor 218 41 28

cr7Color :: V.Color
cr7Color = V.rgbColor 4 106 56


gameFinishedAttr :: AttrName
gameFinishedAttr = "gameFinished"

gameAbortedAttr :: AttrName
gameAbortedAttr = "gameAborted"

gameSelectingAttr :: AttrName
gameSelectingAttr = "gameSelecting"

gameRunningAttr :: AttrName
gameRunningAttr = "gameRunning"

man1Attr, man2Attr,
  boxAttr, boxAttr2, okBoxAttr,
  holeAttr, holeAttr2,
  wallBrickAttr,
  emptyAttr, emptyAttr2,
  welcomeCharAttr, welcomeCharAttr2,
  hardLvAttr, easyLvAttr, mediumLvAttr :: AttrName
man1Attr = "man1Attr"
man2Attr = "man2Attr"
holeAttr = "holeAttr"
holeAttr2 = "holeAttr2"
boxAttr = "boxAttr"
boxAttr2 = "boxAttr2"
okBoxAttr = "okBoxAttr"
wallBrickAttr = "wallBrickAttr"
emptyAttr = "emptyAttr"
emptyAttr2 = "emptyAttr2"
welcomeCharAttr = "welcomCharAttr"
welcomeCharAttr2 = "welcomeCharAttr2"
hardLvAttr = "hardLvAttr"
easyLvAttr = "easyLvAttr"
mediumLvAttr = "mediumLvAttr"

