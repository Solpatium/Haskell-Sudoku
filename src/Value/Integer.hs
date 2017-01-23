module Value.Integer (
  IntegerValue,
    fromInt,
    getAll,
) where

import SudokuAbstract

newtype IntegerValue = IntegerValue Integer
                   deriving (Show, Eq)

instance SudokuValue IntegerValue where
  fromInt 1 = IntegerValue 1
  fromInt 2 = IntegerValue 2
  fromInt 3 = IntegerValue 3
  fromInt 4 = IntegerValue 4
  fromInt 5 = IntegerValue 5
  fromInt 6 = IntegerValue 6
  fromInt 7 = IntegerValue 7
  fromInt 8 = IntegerValue 8
  fromInt 9 = IntegerValue 9
  getAll = map IntegerValue [1..9]
