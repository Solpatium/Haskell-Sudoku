module Board.Vector (
  VectorBoard
) where

import SudokuAbstract
import Data.Vector

squareIndex :: Index2D -> Int
squareIndex (x,y) = y*9+x

-- |VectorBoard is just a wrapped vector. It should be faster than ListBoard, because random access is O(1).
newtype VectorBoard square = VectorBoard (Vector square)

getByIndexes vec [] = []
getByIndexes vec (i:xs) = (vec ! i) : (getByIndexes vec xs)

instance SudokuBoard VectorBoard where
  element (VectorBoard vec) index2d = vec ! squareIndex index2d
  replace (VectorBoard vec) index2d value = VectorBoard $ vec // [(index,value)]
                                             where
                                               index = squareIndex index2d
  getRow (VectorBoard vec) index = squaresValues $ getByIndexes vec indexes
                                   where
                                     indexes = [9*index+i | i<-[0..8]]
  getColumn (VectorBoard vec) index = squaresValues $ getByIndexes vec indexes
                                   where
                                     indexes = [index+i*9 | i<-[0..8]]
  getGroup (VectorBoard vec) index = squaresValues $ getByIndexes vec indexes
                                   where
                                     indexes = [ topCorner+9*i+j | i<-[0,1,2], j<-[0,1,2] ]
                                     topCorner = (index `mod` 3)*3+(index `quot` 3)*3*9
  fromIntList list = if Prelude.length list /= 81
                     then error "Sudoku must have 81 elements"
                     else VectorBoard $ fromList $ Prelude.map toSquare list
                     where
                       toSquare 0 = unknown
                       toSquare n = known (fromInt n)
