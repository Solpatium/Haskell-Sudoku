{-#LANGUAGE KindSignatures #-}

module SudokuAbstract (
    Index,
    Index2D,
      nextIndex,
    SudokuValue,
      fromInt,
      getAll,
    SudokuSquare,
      value,
      isKnown,
      known,
      unknown,
    SudokuBoard,
      element,
      replace,
      getRow,
      getColumn,
      getGroup,
      fromIntList,
    squaresValues,
    valKnown,
    possibilities,
    readBoard
) where

import Data.Maybe
import Data.List

-- |Index represents a group, column or a row
type Index = Int

-- |Index represents a square in 2D
type Index2D = (Index,Index)

-- |nextIndex returns next Index2D
nextIndex (8,y) = (0,y+1)
nextIndex (x,y) = (x+1,y)

-- |SudokuValue is a class of types representing sudoku values (1-9).
class (Show t, Eq t) => SudokuValue t where
  fromInt :: Int -> t
  getAll :: [t]

-- |SudokuSquare is a class of types representing a square in susoku (it either has a value, or it is empty).
class SudokuSquare (t :: * -> *) where
  -- |Returns value wrapped in Maybe. Returns Nothing if value is not known.
  value :: (SudokuValue value) => t value -> Maybe value
  -- |Returns true if value is known, otherwise false
  isKnown :: (SudokuValue value) => t value -> Bool
  -- |Takes a SudokuValue and returns a square with this value
  known :: (SudokuValue value) => value -> t value
  -- |Returns unknown square
  unknown :: (SudokuValue value) => t value

-- |squaresValues takes a list of squares and returns their values.
squaresValues :: (SudokuSquare s, SudokuValue v) => [s v] -> [v]
squaresValues squares = map fromJust $ filter isJust $ map value squares

-- |SudokuBoard is a class of types representing sudoku board.
class SudokuBoard (t :: * -> *) where
  -- |Returns square from given position.
  element :: (SudokuSquare square, SudokuValue val) => t (square val) -> Index2D -> square val
  -- |Replace square at given position with a new square
  replace :: (SudokuSquare square, SudokuValue val) => t (square val) -> Index2D -> square val -> t (square val)
  -- |Returns list of values from given row
  getRow :: (SudokuSquare square, SudokuValue val) => t (square val) -> Index -> [val]
  -- |Returns list of values from given column
  getColumn :: (SudokuSquare square, SudokuValue val) => t (square val) -> Index -> [val]
  -- |Returns list of values from given group (square 3x3)
  getGroup :: (SudokuSquare square, SudokuValue val) => t (square val) -> Index -> [val]
  -- |Takes a list of values in int representation (0-9) and returns a board with those values
  fromIntList :: (SudokuSquare square, SudokuValue val) => [Int] -> t (square val)

-- |Helper function, returns true if square at given position is known
valKnown board index = isKnown $ element board index

-- |possibilities takes a board, index2d and returns possible values for the square
possibilities :: (SudokuBoard b, SudokuSquare s, SudokuValue v) => b (s v) -> Index2D -> [v]
possibilities board (x,y) = filter condition getAll
                            where
                              groupIndex = (x `div` 3) + (y `div` 3)*3
                              group = getGroup board groupIndex
                              column = getColumn board x
                              row = getRow board y
                              condition e = notIn row && notIn column && notIn group
                                where
                                  notIn = notElem e

-- |Helper function, takes a string and read digits from it
readDigits :: String -> [Int]
readDigits string = map readChar $ filter (\c -> elem c ['0'..'9']) string
                   where readChar c = read [c]

-- |Takes a string and returns a filled board
readBoard string = if isValid board
                   then Just board
                   else Nothing
                   where board = fromIntList.readDigits $ string

-- |Checks if board is valid
isValid :: (SudokuBoard b, SudokuSquare s, SudokuValue v) => b (s v) -> Bool
isValid board = all noDuplicates $ concat [ [row i, column i, group i] | i<-[0..8] ]
                 where
                   row = getRow board
                   column = getColumn board
                   group = getGroup board
                   noDuplicates list = (length list) == (length $ nub list)
