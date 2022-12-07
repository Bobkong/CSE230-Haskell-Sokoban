import GameComponent
import GameController
import Paths_CSE230_Haskell_Sokoban
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import UI

main :: IO ()
main = do
  w <- readFilCreateWorld
  defaultMain
    [ testCase ("1.  test initialWorld (expected to be GameReady) -- " ++ (show (state w))) (worldInitTest w),
      testCase ("2.  test restartWorld (expect to be GameRunning) -- " ++ (show (state (restartWorld w)))) (worldRestartTest w),
      testCase ("3.  test changeToIndexWorld (expect to be 0) -- " ++ (show (currentInitGameDataIx (changeToIndexWorld 0 w)))) (indexChangeTest w),
      testCase ("4.  test judgeGameFinishedTest 1") (judgeGameFinishedFalseTest w),
      testCase ("5.  test judgeGameFinishedTest 2") (judgeGameFinishedTrueTest w),
      testCase ("6.  test processManTryMove (judgeManChangeDirectionTest)") (judgeManChangeDirectionTest w),
      testCase ("7.  test processManTryMove (judgeManMoveBlockedTest)") (judgeManMoveBlockedTest w),
      testCase ("8.  test processManTryMove (judgeManMoveSuccessTest)") (judgeManMoveSuccessTest w),
      testCase ("9.  test processMan2TryMove (judgeMan2ChangeDirectionTest)") (judgeMan2ChangeDirectionTest w),
      testCase ("10. test processMan2TryMove (judgeMan2MoveBlockedTest)") (judgeMan2MoveBlockedTest w),
      testCase ("11. test processMan2TryMove (judgeMan2MoveSuccessTest)") (judgeMan2MoveSuccessTest w),
      testCase ("12. test updateOneInBoxesTest") (updateOneInBoxesTest w),
      testCase ("13. test getAdjPtOnDirTest") (getAdjPtOnDirTest w),
      testCase ("14. test judgeMenCollisionTest (judgeMenCollisionTest)") (judgeMenCollisionTest w),
      testCase ("15. test boxMoveSuccessTest (judgeMenCollisionTest)") (boxMoveSuccessTest w),
      testCase ("16. test boxMoveCollisionBoxTest (judgeMenCollisionTest)") (boxMoveCollisionBoxTest w),
      testCase ("17. test boxMoveCollisionWallTest (judgeMenCollisionTest)") (boxMoveCollisionWallTest w)
    ]

worldInitTest :: World -> Assertion
worldInitTest w = assertBool ("State of world is" ++ (show (state w)) ++ " , but expect to be GameReady") (state w == GameReady)

worldRestartTest :: World -> Assertion
worldRestartTest w = assertBool ("State of world is" ++ (show (state (restartWorld w))) ++ " , but expect to be GameRunning") (state (restartWorld w) == GameRunning)

indexChangeTest :: World -> Assertion
indexChangeTest w = assertBool ("Index of selection is" ++ (show testIndex) ++ " , but expect to be 0") (0 == testIndex)
  where
    testIndex = currentInitGameDataIx (changeToIndexWorld 0 w)

judgeGameFinishedFalseTest :: World -> Assertion
judgeGameFinishedFalseTest w = assertBool "judgeGameFinished not work correctly" (False == judgeGameFinished w)

judgeGameFinishedTrueTest :: World -> Assertion
judgeGameFinishedTrueTest w = assertBool "judgeGameFinished not work correctly" (True == judgeGameFinished (w {boxes = [mkBox (1, 1)], holes = [mkHole (1, 1)]}))

judgeManChangeDirectionTest :: World -> Assertion
judgeManChangeDirectionTest w =
  assertBool "test Man move direction failed" $
    DirectDown == direct (head $ man (processManTryMove DirectDown w))

judgeManMoveBlockedTest :: World -> Assertion
judgeManMoveBlockedTest w =
  assertBool "test Man move with obstacles failed" $
    (3, 1) == positionOfMan (head $ man (processManTryMove DirectLeft (w {man = [mkMan (3, 1)], wallBricks = [mkWallBrick (2, 1)]})))

judgeManMoveSuccessTest :: World -> Assertion
judgeManMoveSuccessTest w =
  assertBool "test Man move without obstacles failed" $
    (2, 1) == positionOfMan (head $ man (processManTryMove DirectLeft (w {man = [mkMan (3, 1)]})))

judgeMan2ChangeDirectionTest :: World -> Assertion
judgeMan2ChangeDirectionTest w =
  assertBool "test Man 2 move direction failed" $
    DirectDown == direct (last $ man (processMan2TryMove DirectDown w))

judgeMan2MoveBlockedTest :: World -> Assertion
judgeMan2MoveBlockedTest w =
  assertBool "test Man 2 move with obstacles failed" $
    (3, 1) == positionOfMan (last $ man (processMan2TryMove DirectLeft (w {man = [mkMan (5, 1), mkMan (3, 1)], wallBricks = [mkWallBrick (2, 1)]})))

judgeMan2MoveSuccessTest :: World -> Assertion
judgeMan2MoveSuccessTest w =
  assertBool "test Man 2 move without obstacles failed" $
    (2, 1) == positionOfMan (last $ man (processMan2TryMove DirectLeft (w {man = [mkMan (5, 1), mkMan (3, 1)]})))

updateOneInBoxesTest :: World -> Assertion
updateOneInBoxesTest w =
  assertBool "function updateOneInBoxes works incorrectly" $
    (2, 1) `elem` (map positionOfBox (updateOneInBoxes (boxes (w {boxes = [mkBox (1, 1)]})) (1, 1) (2, 1)))

getAdjPtOnDirTest :: World -> Assertion
getAdjPtOnDirTest w =
  assertBool "function getAdjPtOnDir works incorrectly" $
    (1, 1) == getAdjPtOnDir DirectLeft (2, 1)

judgeMenCollisionTest :: World -> Assertion
judgeMenCollisionTest w =
  assertBool "test Man 2 move with obstacles failed" $
    (3, 1) == positionOfMan (last $ man (processMan2TryMove DirectLeft (w {man = [mkMan (2, 1), mkMan (3, 1)]})))
      && (2, 1) == positionOfMan (head $ man (processManTryMove DirectRight (w {man = [mkMan (2, 1), mkMan (3, 1)]})))

boxMoveSuccessTest :: World -> Assertion
boxMoveSuccessTest w =  assertBool "test box move without obstacles failed" 
    $ (1, 1) `elem` (map positionOfBox (boxes (processManTryMove DirectLeft (w {man = [mkMan (3, 1)], boxes = [mkBox (2, 1)], wallBricks = []}))))

boxMoveCollisionBoxTest :: World -> Assertion
boxMoveCollisionBoxTest w =  assertBool "test box move collision with other boxes failed" 
    $ (2, 1) `elem` boxesPositionAfterMove && ((1, 1) `notElem` boxesPositionAfterMove)
    where boxesPositionAfterMove = (map positionOfBox (boxes (processManTryMove DirectLeft (w {man = [mkMan (4, 1)], boxes = [mkBox (3, 1), mkBox (2, 1)], wallBricks = []}))))


boxMoveCollisionWallTest :: World -> Assertion
boxMoveCollisionWallTest w =  assertBool "test box move collision with wall bricks failed" 
    $ (2, 1) `elem` boxesPositionAfterMove && ((1, 1) `notElem` boxesPositionAfterMove)
    where boxesPositionAfterMove = (map positionOfBox (boxes (processManTryMove DirectLeft (w {man = [mkMan (3, 1)], boxes = [mkBox (2, 1)], wallBricks = [mkWallBrick (1, 1)]}))))