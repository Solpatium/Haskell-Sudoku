-- |Just safeHead/Tail implementation
module List
 (safeHead
 ,safeTail
 ) where


safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]

-- | Returns Just head if the list is not empty or Nothing otherwise
safeHead [] = Nothing
safeHead (h:t) = Just h


-- | Returns Just tail if the list is not empty or Nothing otherwise
safeTail [] = Nothing
safeTail (h:t) = Just t