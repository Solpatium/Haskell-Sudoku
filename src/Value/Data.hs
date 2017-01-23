module Value.Data (
  DataValue,
    fromInt,
    getAll,
    show
) where

import SudokuAbstract

data DataValue = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
                 deriving Eq

instance Show DataValue where
  show One = "1"
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"

instance SudokuValue DataValue where
  fromInt 1 = One
  fromInt 2 = Two
  fromInt 3 = Three
  fromInt 4 = Four
  fromInt 5 = Five
  fromInt 6 = Six
  fromInt 7 = Seven
  fromInt 8 = Eight
  fromInt 9 = Nine
  getAll = [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]
