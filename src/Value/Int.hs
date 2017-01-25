{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Value.Int (
  IntValue,
    fromInt,
    getAll,
) where

import SudokuAbstract
import GHC.Generics (Generic)
import Control.DeepSeq

newtype IntValue = IntValue Int
                   deriving (Show, Eq, Generic, NFData)

instance SudokuValue IntValue where
  fromInt n | n < 1 || n > 9 = error "Invalid cell value"
            | otherwise = IntValue n
  getAll = map IntValue [1..9]
