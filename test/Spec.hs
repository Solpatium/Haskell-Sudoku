import Control.Exception
import Control.Monad
import Test.HUnit
import Board.Vector
import Board.List
import SudokuAbstract
import SimpleSolve
import Square.Simple
import Value.Integer
import Data.Maybe
import Data.Char
import ProjectEuler
import Test.QuickCheck

-- Helper function
boardToMaybeValList :: (SudokuBoard b, SudokuSquare s, SudokuValue v) => b (s v) -> [v]
boardToMaybeValList board = concat $ map (getRow board) [ i | i <-[0..8] ]

-- |Test for solving algorithm - does it work correct?
testSolving :: Test
testSolving = TestCase $ assertEqual "Incorrect solution" vb1 vb2
                where
                  solved    = "483921657967345821251876493548132976729564138136798245372689514814253769695417382"
                  notSolved = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
                  readBoard' string = (fromJust $ readBoard string) :: ListBoard (SimpleSquare IntegerValue)
                  b1 = readBoard' solved
                  b2 = readBoard' notSolved
                  vb1 = boardToMaybeValList b1
                  vb2 = boardToMaybeValList $ fromJust $ solve b2

-- |Test for ProjectEuler algorithm - does it work properly?
testProjectEuler :: Test
testProjectEuler = TestCase $ assertEqual "Wrong answer" (483+483) answer
                   where
                     solved = "483921657967345821251876493548132976729564138136798245372689514814253769695417382"
                     readBoard' string = (fromJust $ readBoard string) :: ListBoard (SimpleSquare IntegerValue)
                     board = readBoard' solved
                     answer = projectEuler96 [Just board, Just board]

-- |Test for reader, it should allow creating invalid sudoku (repeating values)
testReader :: Test
testReader = TestCase $ assertEqual "Parsing invalid sudoku" True (isNothing board)
             where
               wrongInput = "303020600900305001001806400008102900700000008006708200002609500800203009005010300"
               board = readBoard wrongInput :: Maybe( ListBoard (SimpleSquare IntegerValue) )



newtype List9 = List9 [Char] deriving (Eq, Show)
instance Arbitrary List9 where
  arbitrary = do
    list <- shuffle ['1'..'9']
    return (List9 list)

-- |Test for ListBoard, check if it returns the right row values
testListRow :: List9 -> Bool
testListRow (List9 list) = all id $ allElements
                            where
                              input = list ++ (take 72 $ repeat '0')
                              board = (fromJust $ readBoard input) :: ListBoard (SimpleSquare IntegerValue)
                              intList = map digitToInt list
                              values = map fromInt intList
                              row = getRow board 0
                              allElements = map (`elem` row) values

-- |Test for VectorBoard, check if it returns the right row values
testVectorRow :: List9 -> Bool
testVectorRow (List9 list) = all id $ allElements
                            where
                              input = list ++ (take 72 $ repeat '0')
                              board = (fromJust $ readBoard input) :: VectorBoard (SimpleSquare IntegerValue)
                              intList = map digitToInt list
                              values = map fromInt intList
                              row = getRow board 0
                              allElements = map (`elem` row) values

-- |Test for VectorBoard, check if it returns the right column values
testVectorColumn :: List9 -> Bool
testVectorColumn (List9 list) = all id $ allElements
                            where
                              input = concat $ map (:"00000000") list
                              board = (fromJust $ readBoard input) :: VectorBoard (SimpleSquare IntegerValue)
                              intList = map digitToInt list
                              values = map fromInt intList
                              column = getColumn board 0
                              allElements = map (`elem` column) values

main = do
  putStrLn "QuickCheck"
  quickCheckResult testListRow
  quickCheckResult testVectorRow
  quickCheckResult testVectorColumn
  putStrLn "HUnit"
  runTestTT $ TestList [testSolving, testProjectEuler, testReader]
