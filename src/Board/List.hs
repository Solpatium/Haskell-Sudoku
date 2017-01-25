{-# LANGUAGE FlexibleInstances #-} -- This is needed to create custom instance of Show for ListBoard

module Board.List (
  ListBoard
) where

import Data.Maybe
import Data.List
import SudokuAbstract

-- |Helper function, Takes a Index2D and returns index in list
squareIndex :: Index2D -> Int
squareIndex (x,y) = y*9+x

-- |Helper function, replaces element in list at given position
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

-- |Helper function, returns elements from list which indexes satisfy the condition
takeIfIndex f list = foldl' (\acc (v,i) -> if f i then v:acc else acc) [] $ zip list [0..80]

-- |ListBoard is just a wrapped list
newtype ListBoard square = ListBoard [square]

instance (SudokuSquare s, SudokuValue v) => Show (ListBoard (s v)) where
  show (ListBoard list) = loop list
                          where
                             loop [] = ""
                             loop list = "\n" ++ (concat $ map show' $ (take 9 list)) ++ (loop (drop 9 list))
                             show' el | isKnown el = show $ fromJust $ value el
                                      | otherwise = "0"

instance SudokuBoard ListBoard where
  replace   (ListBoard list) index2d value = ListBoard $ replaceAtIndex (squareIndex index2d) value list
  element   (ListBoard list) index2d = list !! (squareIndex index2d)
  getRow    (ListBoard list) index = squaresValues $ takeIfIndex (\i -> index*9<=i && i<index*9+9) list
  getColumn (ListBoard list) index = squaresValues $ takeIfIndex (\i -> i `mod` 9 == index) list
  getGroup  (ListBoard list) index = squaresValues $ takeIfIndex condition list
                                      where topCorner = (index `mod` 3)*3+(index `quot` 3)*3*9
                                            condition i = elem i [ topCorner+9*i+j | i<-[0,1,2], j<-[0,1,2] ]
  fromIntList list = if length list /= 81
                     then error "Sudoku must have 81 elements"
                     else ListBoard $ map toSquare list
                     where
                       toSquare 0 = unknown
                       toSquare n = known (fromInt n)
