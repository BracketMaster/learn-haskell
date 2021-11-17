isPal []   = True
isPal list = if head list == last list
             then isPal (init (tail list))
             else False
-- ghci> isPal [1,2,3,3,2,1]