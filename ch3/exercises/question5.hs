-- $ghci
-- ghci> :l question5.hs 
-- [1 of 1] Compiling Main             ( question5.hs, interpreted )
-- Ok, one module loaded.
-- ghci> isPal [1,2,3,3,2,1]
-- True

isPal []   = True
isPal list = if head list == last list
             then isPal (init (tail list))
             else False