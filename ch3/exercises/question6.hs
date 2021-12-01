-- $ghci
-- ghci> import Data.List
-- ghci> :l question6.hs
-- ghci> listoflists = [[1,2,3], [4,5], [6]]
-- ghci> sortBy sortListofLists listoflists 
-- [[6],[4,5],[1,2,3]]

sortListofLists list1 list2
  | (len1) >  (len2) = GT
  | (len1) <  (len2) = LT
  | (len1) == (len2) = EQ
  where
    len1 = length list1
    len2 = length list2
