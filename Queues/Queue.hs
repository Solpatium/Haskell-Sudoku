{-|
Module      : Queue
Description : Implementation of amortized Queue
Copyright   : (c) Wiesław Stanek 2017
License     : GPL-3
-}
module Queue
  ( Queue
  , queue
  , emptyQ   -- :: Queue a, O(1)
  , isEmptyQ -- :: Queue a -> Bool, O(1)
  , addQ     -- :: a -> Queue a -> Queue a , amortized O(1)
  , remQ     -- :: Queue a -> Maybe (a, Queue a), O(1)
  , fromListQ
  ) where

--interface
  
emptyQ   :: Queue a
queue    :: [a] -> Int -> [a] -> Int -> Queue a
isEmptyQ :: Queue a -> Bool
addQ     :: a -> Queue a -> Queue a
remQ     :: Queue a -> Maybe (a, Queue a)
fromListQ:: [a] -> Queue a
  
--implementation 
-- |Invariance: @flen >= rlen@
data Queue a= MkQueue {f::[a],flen::Int,r::[a],rlen::Int} deriving Show

-- |Creates an empty Queue
emptyQ = MkQueue [] 0 [] 0

-- |Returns True if Queue is empty and False otherwise
isEmptyQ q = flen q + rlen q == 0

-- |Pseudoconstructor that assure that the Queue is amortised
queue f flen r rlen =
 if flen < rlen then
   MkQueue (f++(reverse r)) (flen+rlen) [] 0
 else
  MkQueue f flen r rlen

-- | Adds x to the Queue
addQ x q
 | isEmptyQ q = MkQueue [x] 1 [] 0
 | otherwise = queue (f q) (flen q) (x:(r q)) ((rlen q)+1)
 
-- | Removes first element from the Queue
remQ q
 | isEmptyQ q = Nothing
 | otherwise = Just (head (f q),queue (drop 1 (f q)) ((flen q)-1) (r q) (rlen q))
 
-- | Creates Queue based on the given List
fromListQ [] = emptyQ
fromListQ x = MkQueue x (length x) [] 0