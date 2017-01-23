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

hardest = map (\c-> if c == '.' then '0' else c) ".....6....59.....82....8....45........3........6..3.54...325..6.................."
hard = "800000000003600000070090200050007000000045700000100030001000068008500010090000400"
ok = "123456789000000000000000000000000000000000000000000000000000000000000000000000000"
empty = "000000000000000000000000000000000000000000000000000000000000000000000000000000000"
wrong = "220000000000000000000000000000000000000000000000000000000000000000000000000000000"

intOk = readBoard ok :: VectorBoard (SimpleSquare IntValue)
intEmpty = readBoard empty :: VectorBoard (SimpleSquare IntValue)
dataOk = readBoard ok :: ListBoard (SimpleSquare DataValue)
dataEmpty = readBoard empty :: ListBoard (SimpleSquare DataValue)
charOk = readBoard ok :: ListBoard (SimpleSquare CharValue)
charEmpty = readBoard empty :: ListBoard (SimpleSquare CharValue)
integerOk = readBoard ok :: ListBoard (SimpleSquare IntegerValue)
integerEmpty = readBoard empty :: ListBoard (SimpleSquare IntegerValue)


-- main = defaultMain [ bgroup "DataValue" [ bench "ok"  $ whnf solve dataOk, bench "empty"  $ whnf solve dataEmpty ], bgroup "IntValue" [ bench "ok"  $ whnf solve intOk, bench "empty"  $ whnf solve intEmpty ], bgroup "CharValue" [ bench "ok"  $ whnf solve charOk, bench "empty"  $ whnf solve charEmpty ], bgroup "IntegerValue" [ bench "ok"  $ whnf solve integerOk, bench "empty"  $ whnf solve integerEmpty ]]
benchmarks = defaultMain [ bgroup "IntValue" [ bench "ok"  $ whnf solve intOk, bench "empty"  $ whnf solve intEmpty ]]
