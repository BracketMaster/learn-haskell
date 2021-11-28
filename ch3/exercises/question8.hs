-- $ghci
-- ghci> :l question8.hs 
-- [1 of 1] Compiling Main             ( question8.hs, interpreted )
-- Ok, one module loaded.
-- ghci> passed
-- True

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

tree_depth tree = max_depth 0 tree
    where max_depth acc Empty = acc
          max_depth acc (Node _ lhs rhs) = max height_lhs height_rhs
            where height_lhs = max_depth (acc + 1) lhs
                  height_rhs = max_depth (acc + 1) rhs

test_vector = [(tree7, 3),
               (tree6, 3),
               (tree5, 3),
               (tree4, 3),
               (tree3, 2),
               (tree2, 2),
               (tree1, 1)
               ]
tester (tree, expected_height) = (tree_depth tree) == expected_height
passed = all tester test_vector

tree7 = n1
    where
        n7 = Node 7 Empty Empty
        n6 = Node 6 Empty Empty
        n5 = Node 5 Empty Empty
        n4 = Node 4 Empty Empty
        n3 = Node 3 n6 n7
        n2 = Node 2 n4 n5
        n1 = Node 1 n2 n3

tree6 = n1
    where
        n6 = Node 6 Empty Empty
        n5 = Node 5 Empty Empty
        n4 = Node 4 Empty Empty
        n3 = Node 3 n6 Empty
        n2 = Node 2 n4 n5
        n1 = Node 1 n2 n3

tree5 = n1
    where
        n5 = Node 5 Empty Empty
        n4 = Node 4 Empty Empty
        n3 = Node 3 Empty Empty
        n2 = Node 2 n4 n5
        n1 = Node 1 n2 n3

tree4 = n1
    where
        n4 = Node 4 Empty Empty
        n3 = Node 3 Empty Empty
        n2 = Node 2 n4 Empty
        n1 = Node 1 n2 n3

tree3 = n1
    where
        n3 = Node 3 Empty Empty
        n2 = Node 2 Empty Empty
        n1 = Node 1 n2 n3

tree2 = n1
    where
        n2 = Node 2 Empty Empty
        n1 = Node 1 n2 Empty

tree1 = n1
    where
        n1 = Node 1 Empty Empty