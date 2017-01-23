module Value.Char (
  CharValue,
    fromInt,
    getAll,
) where

import SudokuAbstract

newtype CharValue = CharValue Char
                   deriving (Show, Eq)

instance SudokuValue CharValue where
  fromInt 1 = CharValue '1'
  fromInt 2 = CharValue '2'
  fromInt 3 = CharValue '3'
  fromInt 4 = CharValue '4'
  fromInt 5 = CharValue '5'
  fromInt 6 = CharValue '6'
  fromInt 7 = CharValue '7'
  fromInt 8 = CharValue '8'
  fromInt 9 = CharValue '9'
  getAll = map CharValue ['1'..'9']
