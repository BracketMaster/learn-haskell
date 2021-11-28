data BookInfo = Book Int String [String]
                deriving (Show)

data BookReview = BookReview BookInfo CustomerID String
type CustomerID = Int
type ReviewBody = String

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

badExample (x:xs) = x + badExample xs
main = putStrLn "Hello, world."