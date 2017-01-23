module Square.Simple (
  SimpleSquare
) where

import SudokuAbstract

-- |SimpleSquare does practically the same thing as Maybe, there is either a Known value, or it is Unknown.
data SimpleSquare val = Known val | Unknown

instance (SudokuValue v) => Show (SimpleSquare v) where
  show (Known val) = show val
  show Unknown = "0"

instance SudokuSquare SimpleSquare where
  value (Known val)  = Just val
  value (Unknown) = Nothing
  isKnown (Known val) = True
  isKnown (Unknown) = False
  known val = Known val
  unknown = Unknown
