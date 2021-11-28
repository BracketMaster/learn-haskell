-- $ghci
-- ghci> :l question4.hs 
-- [1 of 1] Compiling Main             ( question4.hs, interpreted )
-- Ok, one module loaded.
-- ghci> pal [1,2,3]
-- [1,2,3,3,2,1]

-- attempt 1 : not efficient

-- rev (x:xs) = rev xs ++ [x]
-- rev []     = []

-- attempt 2 : more efficient

-- rev  = fun []
--     where
--         fun acc []     = acc
--         fun acc (x:xs) = fun (x:acc) xs

-- attempt 3 : equivalent expression of 2

rev :: Foldable t => t a -> [a]
rev = foldl (\acc x -> x : acc) []

pal list = list ++ rev list