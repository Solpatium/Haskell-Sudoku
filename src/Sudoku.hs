module Sudoku (
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

hardest = map (\c-> if c == '.' then '0' else c) ".....6....59.....82....8....45........3........6..3.54...325..6.................."
hard = "800000000003600000070090200050007000000045700000100030001000068008500010090000400"
ok = "123456789000000000000000000000000000000000000000000000000000000000000000000000000"
empty = "000000000000000000000000000000000000000000000000000000000000000000000000000000000"
wrong = "220000000000000000000000000000000000000000000000000000000000000000000000000000000"

intOk = fromJust $ readBoard ok :: VectorBoard (SimpleSquare IntValue)
intEmpty = fromJust $ readBoard empty :: VectorBoard (SimpleSquare IntValue)
dataOk = fromJust $ readBoard ok :: ListBoard (SimpleSquare DataValue)
dataEmpty = fromJust $ readBoard empty :: ListBoard (SimpleSquare DataValue)
charOk = fromJust $ readBoard ok :: ListBoard (SimpleSquare CharValue)
charEmpty = fromJust $ readBoard empty :: ListBoard (SimpleSquare CharValue)
integerOk = fromJust $ readBoard ok :: ListBoard (SimpleSquare IntegerValue)
integerEmpty = fromJust $ readBoard empty :: ListBoard (SimpleSquare IntegerValue)


-- main = defaultMain [ bgroup "DataValue" [ bench "ok"  $ whnf solve dataOk, bench "empty"  $ whnf solve dataEmpty ], bgroup "IntValue" [ bench "ok"  $ whnf solve intOk, bench "empty"  $ whnf solve intEmpty ], bgroup "CharValue" [ bench "ok"  $ whnf solve charOk, bench "empty"  $ whnf solve charEmpty ], bgroup "IntegerValue" [ bench "ok"  $ whnf solve integerOk, bench "empty"  $ whnf solve integerEmpty ]]
benchmarks = do
    grids <- fmap lines $ readFile "sudoku.txt"
    -- print $ projectEuler96 $ solve' grids readIntValue -- Uncomment this to see the answer to project euler problem ;)
    defaultMain [ bgroup "Values" [ bench "IntValue"      $ whnf solve intOk,
                                    bench "IntegerValue"  $ whnf solve intOk,
                                    bench "readCharValue" $ whnf solve intOk,
                                    bench "readDataValue" $ whnf solve intOk ] ]
    -- defaultMain [ bgroup "Values" [ bench "IntValue"      $ whnf projectEuler96 (solve' grids readIntValue),
    --                                 bench "IntegerValue"  $ whnf projectEuler96 (solve' grids readIntegerValue),
    --                                 bench "readCharValue" $ whnf projectEuler96 (solve' grids readCharValue),
    --                                 bench "readDataValue" $ whnf projectEuler96 (solve' grids readDataValue) ] ]
    where
      whnf' arg = arg
      solve' grids readBoard  = fmap (solve.readBoard) grids
      readBoard' grid = fromJust.readBoard $ grid
      readIntValue grid       = readBoard' grid :: VectorBoard (SimpleSquare IntValue)
      readIntegerValue grid   = readBoard' grid :: VectorBoard (SimpleSquare IntegerValue)
      readCharValue grid      = readBoard' grid :: VectorBoard (SimpleSquare CharValue)
      readDataValue grid      = readBoard' grid :: VectorBoard (SimpleSquare DataValue)
