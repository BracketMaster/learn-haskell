-- $ghci
-- ghci> :l question11.hs 
-- [1 of 1] Compiling Question11       ( question11.hs, interpreted )
-- Ok, one module loaded.
-- ghci> passed
-- True
module Question11 where

data Direction = Left | Right | Straight deriving (Eq, Show)

-- Note that Point2 and Vec2 while structurally identical
-- are semantically different. Point2 is simple a point on
-- the two dimensional cartesian plane. Vec2 is a vector in R2
-- anchored at zero. Mathematically, taking the determinant
-- of two points makes no sense, so here, we only take the
-- determinant of two vectors.
data Point2 = Point2 Integer Integer deriving (Show)
data Vec2   = Vec2   Integer Integer deriving (Show)

-- The following function takes two points, p1 and p2, and
-- returns a vector that when translated to p1 will point to
-- p2.
makeVec (Point2 i1 j1) (Point2 i2 j2) = Vec2 (i2 - i1) (j2 - j1)

-- A positive determinant means that the first vector argument
-- is to the right of the second vector argument.
-- A negative determinant means that the first vector argument
-- is to the left of the second vector argument.
-- Note, it is assumed that the vectors are anchored at the origin.
det2 (Vec2 i1 j1) (Vec2 i2 j2) = i1*j2 - i2*j1

direction_relative_to this_vec ref_vec
    | dir < 0  = Question11.Left
    | dir > 0  = Question11.Right
    | dir == 0 = Question11.Straight
    where
        dir = det2 this_vec ref_vec

-- given a path composed of three points, p1, p2, and p3, we
-- want to know if the path curves left, right, or goes straight
path_direction p1 p2 p3 =
    let vec1 = makeVec p1 p2 in
    let vec2 = makeVec p2 p3 in
    vec2 `direction_relative_to` vec1

path_directions list_of_points@(p1:p2:p3:_) = 
    current_direction:remaining_directions
    where
        current_direction    = path_direction p1 p2 p3
        remaining_directions = path_directions $ tail list_of_points

path_directions _ = []

-- TESTS --
-- a diagram of the following tests is in resources/question11.svg
p0 = Point2 1 0
p1 = Point2 0 1
p2 = Point2 1 4
p3 = Point2 3 4
p4 = Point2 2 3
p5 = Point2 2 2
p6 = Point2 2 1
p7 = Point2 3 3
p8 = Point2 4 2
p9 = Point2 2 0

test_points = [p0, p1, p2, p3, p4, p5, p6, p7, p8,p9]
results = path_directions test_points
expected = [
    Question11.Right,
    Question11.Right,
    Question11.Right,
    Question11.Left,
    Question11.Straight,
    Question11.Left,
    Question11.Right,
    Question11.Right]

test_vector = zip results expected
tester (result, expected) = result == expected
passed = all tester test_vector