-- $ghci
-- ghci> :l question1.hs 
-- [1 of 1] Compiling Main             ( question1.hs, interpreted )
-- Ok, one module loaded.
-- ghci> len [1,2,3]
-- 3

len (x:xs)  = 1 + len xs
len []      = 0