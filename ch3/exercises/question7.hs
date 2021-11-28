-- $ghci
-- ghci> :l question7.hs 
-- [1 of 1] Compiling Main             ( question7.hs, interpreted )
-- Ok, one module loaded.
-- ghci> intersperse ',' ["foo","bar","baz","quux"]
-- "foo,bar,baz,quux"

intersperse :: a -> [[a]] -> [a]
intersperse element list = tail (prepended_with_element list)
    where prepended_with_element = foldl (\acc el -> acc ++ element:el) []