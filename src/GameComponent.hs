module GameComponent where

import Data.List

type Point = (Int, Int)

data Man = Man {
   positionOfMan :: Point
  ,steps :: Int
  ,direct :: Direction
}

mkMan :: Point -> Man
mkMan positionPoint = Man positionPoint 0 DirectUp

data Direction = DirectUp
  | DirectDown
  | DirectLeft
  | DirectRight
  deriving(Show, Eq, Ord)

data Box = Box {
  positionOfBox :: Point
}

mkBox :: Point -> Box 
mkBox positionPoint = Box positionPoint

data Hole = Hole {
  positionOfHole :: Point
}

mkHole :: Point -> Hole 
mkHole positionPoint = Hole positionPoint


data WallBrick = WallBrick {
  positionOfWallBrick :: Point
}

mkWallBrick :: Point -> WallBrick
mkWallBrick positionPoint  = WallBrick positionPoint

data InitData = InitData {
   level :: String
  ,playerNumber :: String
  ,hWidth :: Int
  ,hHeight :: Int
  ,manPos :: [Point]
  ,boxPos :: [Point]
  ,holePos :: [Point]
  ,wallBrickPos :: [Point]
} deriving(Show, Read)

rInitData :: String -> [InitData]
rInitData = read

addBorderProperly :: InitData -> InitData
addBorderProperly prev = prev { wallBrickPos = oldWPos ++ bounds}
  where 
    oldWPos = wallBrickPos prev
    bounds = genBounduaries (hWidth prev) $ hHeight prev


genBounduaries :: Int -> Int -> [Point]
genBounduaries maxX maxY = nub $ leftRightBounds ++ topBottomBounds
  where
    topBottomBounds = 
      foldr1 (++) (map (\f -> map f [(- maxX)..maxX]) [\x -> (x, (-maxY)),\x -> (x,maxY)])
    leftRightBounds = 
      foldr1 (++) (map (\f -> map f [(- maxY)..maxY]) [\y -> (maxX,y), \y -> ((-maxX),y)])

-- inlineSampleInitData :: [InitData]
-- inlineSampleInitData = [sample2, sample1]
--   where 
--     sample1 = InitData "Hard" 3 3 (0,0) [(-1,0),(1,0),(0,-2)] [(1,2),(2,-1),(0,-2)] [(1,1),(1,-1),(-1,-1),(-1,1)]
--     sample2 = InitData "Medium" 3 3 (-1,1) 
--           [(-1,0),(0,0),(1,0),(-1,-1)]  [(0,1),(0,2), (-1,2),(-2,2)] [(-2,0),(1,1),(1,-1)]
