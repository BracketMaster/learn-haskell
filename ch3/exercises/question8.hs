data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

tree_depth tree = max_depth 0 tree
    where
        max_depth acc Empty = acc
        max_depth acc (Node _ Empty Empty) = acc + 1
        max_depth acc (Node _ lhs Empty) = max_depth (acc + 1) lhs
        max_depth acc (Node _ Empty rhs) = max_depth (acc + 1) rhs
        max_depth acc (Node _ lhs rhs) = max height_lhs height_rhs
            where
                height_lhs = max_depth (acc + 1) lhs
                height_rhs = max_depth (acc + 1) rhs


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