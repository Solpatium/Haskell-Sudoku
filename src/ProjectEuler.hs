module ProjectEuler (
  projectEuler96
) where

import Data.Maybe
import Data.Char
import Data.List
import SudokuAbstract

projectEuler96 boards = sum $ fullAnswer boards
                                 where
                                   fullAnswer boards       = fmap (answer.fromJust) boards
                                   answer board            = (read $ map intToDigit $ map (toInt.fromJust.value) $ map (element board) [(0,0),(1,0),(2,0)]) :: Int
                                   toInt :: (SudokuValue v) => v -> Int
                                   toInt val = (+1) $ fromJust $ elemIndex val (map fromInt [1..9])
