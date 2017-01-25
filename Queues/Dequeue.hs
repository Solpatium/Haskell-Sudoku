{-|
Module      : Dequeue
Description : Implementation of amortized Dequeue based on BankersDequeue
Copyright   : (c) Wiesław Stanek 2017
License     : GPL-3
-}
module Dequeue
 ( Dequeue
 , c
 , dequeue
 , emptyDEQ     -- :: Dequeue a
 , isEmptyDEQ   -- :: Dequeue a -> Bool
 , lengthDEQ    -- :: Dequeue a -> Int, O(1)
 , firstDEQ     -- :: Dequeue a -> Maybe a,  O(1)
 , lastDEQ      -- :: Dequeue a -> Maybe a, O(1)
 , takefrontDEQ -- :: Int -> Dequeue a -> [a], O(n)
 , takeBackDEQ  -- :: Int -> Dequeue a -> [a], O(n)
 , pushfrontDEQ -- :: Dequeue a -> a -> Dequeue a, O(1) amortised
 , popfrontDEQ  -- :: Dequeue a -> Maybe (a, Dequeue a), O(1) amortised
 , pushBackDEQ  -- :: Dequeue a -> a -> Dequeue a, O(1) amortised
 , popBackDEQ   -- :: Dequeue a -> Maybe (a, q a), O(1) amortised
 , fromListDEQ  -- :: [a] -> Dequeue a, O(n)
 , extractDEQ
 , toListDEQ
 ) where

import List
import Data.Maybe

-- interface
emptyDEQ :: Dequeue a
dequeue :: [a] -> Int -> [a] -> Int -> Dequeue a
isEmptyDEQ :: Dequeue a -> Bool
lengthDEQ :: Dequeue a -> Int
firstDEQ :: Dequeue a -> Maybe a
lastDEQ :: Dequeue a -> Maybe a
takefrontDEQ :: Int -> Dequeue a -> [a]
takeBackDEQ :: Int -> Dequeue a -> [a]
pushfrontDEQ :: Dequeue a -> a -> Dequeue a
popfrontDEQ :: Dequeue a -> Maybe (a, Dequeue a)
pushBackDEQ :: Dequeue a -> a -> Dequeue a
popBackDEQ :: Dequeue a -> Maybe (a, Dequeue a)
fromListDEQ :: [a] -> Dequeue a
extractDEQ :: Maybe (a, Dequeue a) -> (Maybe a, Dequeue a)
toListDEQ :: Dequeue a -> [a]


-- implementation
-- |Invariants: flength <= c*rlength + 1 and rlength <= c*flength
data Dequeue a = MkDequeue {f :: [a], flength :: Int,
                            r :: [a], rlength :: Int} deriving Show



-- |Balance constant, c == 3
c = 3
-- |Creating empty Dequeue
emptyDEQ = MkDequeue [] 0 [] 0
-- |Pseudoconstructor to assure that Dequeue is balanced
dequeue f flen r rlen = 
 if (flen > c*rlen + 1) then
  let 
  i = div (flen + rlen) 2
  j = flen + rlen - i
  f' = take i f
  r' = r ++ reverse (drop i f)
  in
  MkDequeue f' i r' j
 else if (rlen > c*flen + 1) then
  let 
  i = div (flen + rlen) 2
  j = flen + rlen - i
  f' = f ++ reverse (drop j r)
  r' = take j r
  in
  MkDequeue f' i r' j
 else
  MkDequeue f flen r rlen

-- |Returns the length of Dequeue
lengthDEQ (MkDequeue f flen r rlen) = (flen) + (rlen)

-- |Returns True if the Dequeue is empty
isEmptyDEQ (MkDequeue f flen r rlen) = flen + rlen == 0

-- |Returns first element of the Dequeue
firstDEQ (MkDequeue f flen r rlen) 
 | flen==0 = safeHead r
 | otherwise = safeHead f

-- |Returns last element of the Dequeue
lastDEQ (MkDequeue f flen r rlen)  
 | rlen==0 = safeHead f
 | otherwise = safeHead r
 
-- |Returns the first n elements of the Dequeue in frontpop order
takefrontDEQ n (MkDequeue f flen r rlen) = take n f ++ take i (reverse r)
 where
 i = max (n-flen) 0

-- |Returns the last n elements of the Dequeue in backpop order
takeBackDEQ n (MkDequeue f flen r rlen) = take n r ++ take i (reverse f) 
 where
 i = max (n-rlen) 0

-- |Pushes element to the front of the Dequeue
pushfrontDEQ (MkDequeue f flen r rlen) x = dequeue (x:f) (flen+1) r rlen

-- |Pushes element to the end of the Dequeue
pushBackDEQ (MkDequeue f flen r rlen) x = dequeue f flen (x:r) (rlen +1)

-- |Popes element from the front of the Dequeue
popfrontDEQ (MkDequeue f flen r rlen)
 | rlen + flen ==0 = Nothing
 | rlen==0 = Just (head f,emptyDEQ)
 | flen==0 = Just (head r,emptyDEQ)
 | otherwise = Just (head f, dequeue (drop 1 f) (flen-1) r (rlen))

-- |Popes element from the end of the Dequeue
popBackDEQ (MkDequeue [] 0 [] 0) = Nothing
popBackDEQ (MkDequeue f flen r rlen) 
 | rlen==0 = Just (head f,emptyDEQ)
 | flen==0 = Just (head r,emptyDEQ)
 | otherwise = Just (head r, dequeue f flen (drop 1 r) (rlen - 1))

-- |Create Dequeue based on the given list
fromListDEQ [] = emptyDEQ
fromListDEQ x = MkDequeue f' flen r' rlen
 where
 i = length x
 rlen = div i 2
 flen = i - rlen
 f' = take flen x
 r' = reverse (drop flen x)
 
-- |Extracts the result of pop functions
extractDEQ Nothing = (Nothing, emptyDEQ)
extractDEQ q = (Just (fst a), (snd a))
 where a = fromJust q

-- |Converts Dequeue to list
toListDEQ q
 | isEmptyDEQ q = []
 | otherwise = (f q)++(reverse (r q))