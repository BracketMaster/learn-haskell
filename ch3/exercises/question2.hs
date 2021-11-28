-- $ghci
-- ghci> :l question2.hs 
-- [1 of 1] Compiling Main             ( question2.hs, interpreted )
-- Ok, one module loaded.
-- ghci> len [1,2,3]
-- 3

len :: Num a1 => [a2] -> a1
len (x:xs)  = 1 + len xs
len []      = 0