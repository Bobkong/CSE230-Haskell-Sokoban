module GameController where

import GameComponent

tickTimeMS :: Int
tickTimeMS = 250

data GameState = GameReady
 | GameSelecting
 | GameRunning
 | GameFinished
 | GameAborted
 deriving(Show, Eq)

-- V + C: View and Control
data World = World {
  --  init data and selector
   allInitGameData :: [InitData]
  ,currentInitGameDataIx :: Int 

  -- one play data loaded from init data
  ,areaHalfWidth :: Int
  ,areaHalfHeight :: Int
  ,currentLevel :: String
  ,currentPlayerNum :: String
  ,man :: [Man]
  ,boxes :: [Box]
  ,holes :: [Hole]
  ,wallBricks :: [WallBrick]

  --  changing game state
  ,state :: GameState
  ,runningTicks :: Int
}

-- common func: set state, set initData, set index, reload one play data  
launchWorld :: [InitData]  -> Int -> GameState -> World
launchWorld ad index st = World ad index hw hh lv pNum man boxes holes wallBricks st zeroTicks
  where 
    hw = hWidth iData
    hh = hHeight iData
    lv = level iData
    pNum = playerNumber iData
    man = map mkMan $ manPos iData
    boxes = map mkBox $ boxPos iData
    holes = map mkHole $ holePos iData
    wallBricks = map mkWallBrick $ wallBrickPos iData
    iData = ad !! index -- under strong protection
    zeroTicks = 0
 

-- extra :
-- How to get random number list? gen <- getStdGen in main, then use seed to 
-- generate inifnite list rnds = randomRs (0,20) gen

-- state tasnform on world

-- create world from initData and set state to GameReady
initialWorld :: [InitData] -> World
initialWorld legalInitDataStr = launchWorld legalInitDataStr 0 GameReady
    
-- with same index restart game
restartWorld :: World -> World
restartWorld w = launchWorld (allInitGameData w) (currentInitGameDataIx w) GameRunning  

--  change the index and launch a new corresponding world
changeToIndexWorld :: Int -> World -> World
changeToIndexWorld ix w = launchWorld (allInitGameData w) ix GameSelecting

processTick :: World -> World
processTick w = 
  case st of
    GameRunning ->
      if judgeGameFinished w 
        then w { state = GameFinished } 
        else w { runningTicks = nowTks + 1 }
    GameSelecting -> w { runningTicks = nowTks + 1 }
    _ -> w
    where
      st = state w
      nowTks = runningTicks w

judgeGameFinished :: World -> Bool
judgeGameFinished w = foldr1 (&&) checkList
  where 
    checkList = map (\p -> p `elem` holePos) boxPos
    boxPos = map positionOfBox (boxes w)
    holePos = map positionOfHole (holes w)

processManTryMove :: Direction -> World -> World
processManTryMove dir w 
  | pushPo `elem` wallBrickPos = w { man = cannotMoveMen } -- push wall
  | pushPo `elem` manPos = w { man = cannotMoveMen} -- push another man
  | pushPo `elem` boxPos && (isBoxMovableOnDir dir pushPo w)
          = w {man = canMoveMen, boxes = liveBoxes'} -- push box
  | pushPo `elem` boxPos = w {man = cannotMoveMen} -- cannot push box
  | otherwise = w {man = canMoveMen} -- just walk
    where
      canMoveMen = turnedMan { positionOfMan = pushPo, steps = steps turnedMan + 1} : last (man w) : []
      cannotMoveMen = mman { direct = dir } : last (man w) : []

      turnedMan = mman { direct = dir }

      liveBoxes' = updateOneInBoxes liveBoxes pushPo pushPushPo
      pushPushPo = getAdjPtOnDir dir pushPo

      pushPo = getAdjPtOnDir dir $ positionOfMan mman

      wallBrickPos = map positionOfWallBrick (wallBricks w)
      boxPos = map positionOfBox liveBoxes
      liveBoxes = boxes w
      manPos = map positionOfMan (man w)
      mman = head (man w)

processMan2TryMove :: Direction -> World -> World
processMan2TryMove dir w 
  | pushPo `elem` wallBrickPos = w { man = cannotMoveMen } -- push wall
  | pushPo `elem` manPos = w { man = cannotMoveMen} -- push another man
  | pushPo `elem` boxPos && (isBoxMovableOnDir dir pushPo w)
          = w {man = canMoveMen, boxes = liveBoxes'} -- push box
  | pushPo `elem` boxPos = w {man = cannotMoveMen} -- cannot push box
  | otherwise = w {man = canMoveMen} -- just walk
    where
      canMoveMen = head (man w) : turnedMan { positionOfMan = pushPo, steps = steps turnedMan + 1} : []
      cannotMoveMen = head (man w) : mman { direct = dir } : []

      turnedMan = mman { direct = dir }

      liveBoxes' = updateOneInBoxes liveBoxes pushPo pushPushPo
      pushPushPo = getAdjPtOnDir dir pushPo

      pushPo = getAdjPtOnDir dir $ positionOfMan mman

      wallBrickPos = map positionOfWallBrick (wallBricks w)
      boxPos = map positionOfBox liveBoxes
      liveBoxes = boxes w
      manPos = map positionOfMan (man w)
      mman = last (man w)

isBoxMovableOnDir :: Direction -> Point -> World -> Bool
isBoxMovableOnDir dir boxPo w
  | (judgeP `elem` boxPos) || (judgeP `elem` wallBrickPos) || (((currentPlayerNum w) == "2") && (judgeP `elem` manPos)) = False
  | otherwise = True
    where
      judgeP = getAdjPtOnDir dir boxPo
      wallBrickPos = map positionOfWallBrick (wallBricks w)
      boxPos = map positionOfBox (boxes w)
      manPos =  map positionOfMan (man w)

updateOneInBoxes :: [Box] -> Point -> Point -> [Box]
updateOneInBoxes boxes from to =
  map ( \b@(Box p) -> if p == from
      then b { positionOfBox = to}
      else b
  ) boxes

--  x y translation
getAdjPtOnDir :: Direction -> Point -> Point
getAdjPtOnDir dir (x,y)
 | dir == DirectUp = (x, y + 1)
 | dir == DirectDown = (x, y - 1)
 | dir == DirectLeft = (x - 1, y)
 | dir == DirectRight = (x + 1, y)
