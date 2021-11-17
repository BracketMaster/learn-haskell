len (x:xs)  = 1 + len xs
len []      = 0

-- ghci
-- : l question1.hs
-- len [1,2,3]