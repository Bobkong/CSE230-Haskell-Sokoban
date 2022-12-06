import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import GameComponent
import GameController
import Paths_CSE230_Haskell_Sokoban
import UI

main :: IO ()
main = do
    w <- readFilCreateWorld
    defaultMain [
            testCase ("1. test initialWorld (expected to be GameReady) -- " ++ (show (state w))) (worldInitTest w),
            testCase ("2. test restartWorld (expect to be GameRunning) -- " ++ (show (state (restartWorld w)))) (worldRestartTest w),
            testCase ("3. test changeToIndexWorld (expect to be 0) -- " ++ (show (currentInitGameDataIx (changeToIndexWorld 0 w)))) (indexChangeTest w)
        ]

worldInitTest :: World -> Assertion
worldInitTest w = assertBool ("State of world is" ++ (show (state w)) ++ " , but expect to be GameReady") (state w == GameReady)

worldRestartTest :: World -> Assertion
worldRestartTest w = assertBool ("State of world is" ++ (show (state (restartWorld w))) ++ " , but expect to be GameRunning") (state (restartWorld w) == GameRunning)

indexChangeTest :: World -> Assertion
indexChangeTest w = assertBool ("Index of selection is" ++ (show testIndex) ++ " , but expect to be 0") (0 == testIndex)
    where testIndex = currentInitGameDataIx (changeToIndexWorld 0 w)