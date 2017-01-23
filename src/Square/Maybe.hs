{-# LANGUAGE TypeSynonymInstances #-}

module Square.Maybe (
  MaybeSquare
) where

import SudokuAbstract
import Data.Maybe

-- |MaybeSquare is just an alias of maybe.
-- It is created to check if Maybe is somehow faster than a "custom Maybe" - SimpleSquare.
type MaybeSquare = Maybe

instance SudokuSquare MaybeSquare where
  value square = square
  isKnown square = isJust square
  known val = Just val
  unknown = Nothing
