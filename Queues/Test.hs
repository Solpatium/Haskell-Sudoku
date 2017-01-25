{-# LANGUAGE TemplateHaskell #-}
import Dequeue
import Test.QuickCheck
import Data.Maybe
import Test.HUnit

-- |Check if after pushfront and popfront the dequeue is the same
prop_pushpop_frontDEQ ::[Int] -> Int -> Bool
prop_pushpop_frontDEQ deq x = deq == toListDEQ (snd ( extractDEQ (popfrontDEQ(pushfrontDEQ (fromListDEQ deq) x))))

-- |Check if after pushback and popback the dequeue is the same
prop_pushpop_backDEQ :: [Int] -> Int -> Bool
prop_pushpop_backDEQ deq x = deq == toListDEQ (snd ( extractDEQ (popBackDEQ(pushBackDEQ (fromListDEQ deq) x))))

-- |Check if firstDEQ returns last pushFront element
prop_push_frontDEQ :: [Int] -> Int -> Bool
prop_push_frontDEQ deq x = x == fromJust (firstDEQ (pushfrontDEQ (fromListDEQ deq) x))

-- |Check if lastDEQ returns last pushBack element
prop_push_backDEQ :: [Int] -> Int -> Bool
prop_push_backDEQ deq x = x == fromJust (lastDEQ (pushBackDEQ (fromListDEQ deq) x))

-- |Check if takeFront returns the last n pushed elements
prop_takeFrontDEQ :: [Int] -> [Int] -> Bool
prop_takeFrontDEQ deq xs = (reverse xs) == (takefrontDEQ (length xs) $ foldl pushfrontDEQ (fromListDEQ deq) xs)

-- |Check if takeBack returns the last n pushed elements
prop_takeBackDEQ :: [Int] -> [Int] -> Bool 
prop_takeBackDEQ deq xs = (reverse xs) == (takeBackDEQ (length xs) $ foldl pushBackDEQ (fromListDEQ deq) xs)

-- |Check if the length of generated Dequeue equals to length of the list
prop_length_fromListDEQ :: [Int] -> Bool
prop_length_fromListDEQ deq = length deq == lengthDEQ (fromListDEQ deq)

-- |Check if toListDEQ . fromListDEQ x equals to x
prop_toList_fromListDEQ :: [Int] -> Bool
prop_toList_fromListDEQ deq = deq == (toListDEQ $ fromListDEQ deq)

-- |HUnit test for firstDEQ
testFirstDEQforNonEmpty :: Test
testFirstDEQforNonEmpty = TestCase $ assertEqual "Should return (Just first) for non empty Dequeue"
                          (Just 1) (firstDEQ (fromListDEQ [1,2,3]))
        
-- |HUnit test for lastDEQ
testLastDEQforNonEmpty :: Test
testLastDEQforNonEmpty = TestCase $ assertEqual "Should return (Just last) for non empty Dequeue"
                          (Just 3) (lastDEQ (fromListDEQ [1,2,3]))

-- |HUnit test for isEmptyDEQ
testIsEmptyDEQforEmpty :: Test
testIsEmptyDEQforEmpty = TestCase $ assertEqual "Should return True for empty Dequeue"
                         True (isEmptyDEQ emptyDEQ)

-- |HUnit test for isEmptyDEQ						 
testIsEmptyDEQforNonEmpty :: Test
testIsEmptyDEQforNonEmpty = TestCase $ assertEqual "Should return False for non empty Dequeue"
                            False (isEmptyDEQ (fromListDEQ [1]))
       
-- |HUnit test for lengthDEQ
testLengthDEQforEmpty :: Test
testLengthDEQforEmpty = TestCase $ assertEqual "Should return 0 for empty Dequeue"
                        0 (lengthDEQ emptyDEQ)

-- |HUnit test for lengthDEQ						
testLengthDEQfornonEmpty :: Test
testLengthDEQfornonEmpty = TestCase $ assertEqual "Should return length for non empty Dequeue"
                           5 (lengthDEQ (fromListDEQ [1,2,3,4,5]))

return []
main = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 1000 })
       >> (runTestTT $ TestList [testFirstDEQforNonEmpty,testIsEmptyDEQforEmpty,
                                 testIsEmptyDEQforNonEmpty,testLastDEQforNonEmpty,
                                 testLengthDEQforEmpty,testLengthDEQfornonEmpty])