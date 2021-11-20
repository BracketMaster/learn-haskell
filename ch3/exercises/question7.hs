intersperse :: a -> [[a]] -> [a]
intersperse element list = tail (prepended_with_element list)
    where prepended_with_element = foldl (\acc el -> acc ++ element:el) []

-- ghci> intersperse ',' ["foo","bar","baz","quux"]
-- "foo,bar,baz,quux"