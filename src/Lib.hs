module Lib
  ( testLib )
where

import System.IO  
import Control.Monad

testLib :: IO ()
testLib = do gameConfig <- openFile "gameConfigs/gameData.txt" ReadMode
             contents <- hGetContents gameConfig
             print contents