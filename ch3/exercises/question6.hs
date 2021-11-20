sortListofLists list1 list2
  | (length list1) >  (length list2) = GT
  | (length list1) <  (length list2) = LT
  | (length list1) == (length list2) = EQ

-- ghci> import Data.List
-- ghci> :l question6.hs
-- ghci> listoflists = [[1,2,3], [4,5], [6]]
-- ghci> sortBy sortListofLists listoflists 
-- [[6],[4,5],[1,2,3]]