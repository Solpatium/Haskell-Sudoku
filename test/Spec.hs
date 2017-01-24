import Control.Exception
import Control.Monad
import Test.HUnit
import Board.List
import SudokuAbstract
import SimpleSolve
import Square.Simple
import Value.Integer

assertRaises :: (Show a, Control.Exception.Exception e, Show e, Eq e) =>
                String -> e -> IO a -> IO ()


assertRaises msg selector action =
    let thetest e = if e == selector then return ()
                    else assertFailure $ msg ++ "\nReceived unexpected exception: "
                             ++ (show e) ++ "\ninstead of exception: " ++ (show selector)
        in do
           r <- Control.Exception.try action
           case r of
                  Left e -> thetest e
                  Right _ -> assertFailure $ msg ++ "\nReceived no exception, but was expecting exception: " ++ (show selector)

assertError :: Show a => String -> String -> a -> IO()
assertError msg ex f = assertRaises msg (ErrorCall ex) $ evaluate f

testAlgorithm :: Test
testAlgorithm = TestCase $ assertEqual "Incorrect solution"
                (Just (readBoard "483921657967345821251876493548132976729564138136798245372689514814253769695417382" :: ListBoard(SimpleSquare IntegerValue))) 
				(solve(readBoard "003020600900305001001806400008102900700000008006708200002609500800203009005010300" :: ListBoard(SimpleSquare IntegerValue)))


testReader :: Test
testReader = TestCase $ assertError "Error throwing" "Invalid board" (readBoard "303020600900305001001806400008102900700000008006708200002609500800203009005010300" :: ListBoard(SimpleSquare IntegerValue))
             

--main :: IO ()
main = runTestTT $ TestList [testAlgorithm,testReader]
