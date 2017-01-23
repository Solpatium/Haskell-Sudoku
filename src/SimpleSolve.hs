module SimpleSolve (
  solve
) where

import SudokuAbstract
import Data.Maybe

-- |Very basic alghoritm of solving a sudoku.
solve :: (SudokuBoard b, SudokuSquare s, SudokuValue v) => b (s v) -> Maybe (b (s v))
solve board = solve' board (0,0)
              where
                solve' board (0,9) = Just board
                solve' board index = if valKnown board index
                                     then solve' board (nextIndex index)
                                     else loop (possibilities board index) board index
                                     where
                                       loop [] board index = Nothing
                                       loop (x:xs) board index = if isJust solve
                                                                 then solve
                                                                 else loop xs board index
                                         where
                                           newBoard = replace board index (known x)
                                           solve = solve' newBoard (nextIndex index)
