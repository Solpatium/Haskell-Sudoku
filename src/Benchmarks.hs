module Benchmarks (
  benchmarks
) where

import SudokuAbstract
import Value.Data
import Value.Int
import Value.Char
import Value.Integer
import Square.Simple
import Square.Maybe
import Board.List
import Board.Vector
import SimpleSolve
import Criterion.Main
import Data.Maybe
import ProjectEuler
import Control.DeepSeq

benchmarks = do
    grids <- fmap lines $ readFile "sudoku.txt"
    print $ projectEuler96 $ solve' grids readIntValue -- Uncomment this to see the answer to project euler problem ;)
    defaultMain [ bgroup "Values"  [ bench "IntValue"      $ nf (solve' grids) readIntValue,
                                     bench "IntegerValue"  $ nf (solve' grids) readIntegerValue,
                                     bench "CharValue"     $ nf (solve' grids) readCharValue,
                                     bench "DataValue"     $ nf (solve' grids) readDataValue ],
                  bgroup "Squares" [ bench "SimpleSquare"  $ nf (solve' grids) readSimpleSquare,
                                     bench "MaybeSquare"   $ nf (solve' grids) readMaybeSquare ],
                  bgroup "Boards"  [ bench "VectorBoard"   $ nf (solve' grids) readVectorBoard,
                                     bench "ListBoard"     $ nf (solve' grids) readListBoard ] ]
    where
      solve' grids readBoard = fmap (solve.readBoard) grids
      readBoard' grid = fromJust.readBoard $ grid
      -- Different values
      readIntValue grid     = readBoard' grid :: VectorBoard (SimpleSquare IntValue)
      readIntegerValue grid = readBoard' grid :: VectorBoard (SimpleSquare IntegerValue)
      readCharValue grid    = readBoard' grid :: VectorBoard (SimpleSquare CharValue)
      readDataValue grid    = readBoard' grid :: VectorBoard (SimpleSquare DataValue)
      -- Different squares
      readSimpleSquare grid = readBoard' grid :: VectorBoard (SimpleSquare DataValue)
      readMaybeSquare grid  = readBoard' grid :: VectorBoard (MaybeSquare DataValue)
      -- Different boards
      readVectorBoard grid  = readBoard' grid :: VectorBoard (SimpleSquare DataValue)
      readListBoard grid    = readBoard' grid :: ListBoard (SimpleSquare DataValue)
