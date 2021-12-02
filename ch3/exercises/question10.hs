-- $ghci
-- ghci> :l question10.hs 
-- [1 of 1] Compiling Question10       ( question10.hs, interpreted )
-- Ok, one module loaded.
-- ghci> passed
-- True
module Question10 where

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

-- given a path composed of three points, p1, p2, and p3, we
-- want to know if the path curves left, right, or goes straight
path_direction p1 p2 p3 =
    let vec1 = makeVec p1 p2 in
    let vec2 = makeVec p2 p3 in
    vec2 `direction_relative_to` vec1

-- A positive determinant means that the first vector argument
-- is to the right of the second vector argument.
-- A negative determinant means that the first vector argument
-- is to the left of the second vector argument.
-- Note, it is assumed that the vectors are anchored at the origin.
det2 (Vec2 i1 j1) (Vec2 i2 j2) = i1*j2 - i2*j1

direction_relative_to this_vec ref_vec
    | dir < 0  = Question10.Left
    | dir > 0  = Question10.Right
    | dir == 0 = Question10.Straight
    where
        dir = det2 this_vec ref_vec

path1a = Point2 1 0
path1b = Point2 0 1
path1c = Point2 1 4

path2a = Point2 2 0
path2b = Point2 4 2
path2c = Point2 3 3

path3a = Point2 3 4
path3b = Point2 2 3
path3c = Point2 2 1

res1 = path_direction path1a path1b path1c
res2 = path_direction path2a path2b path2c
res3 = path_direction path3a path3b path3c

test_vector = [
    (res1, Question10.Right),
    (res2, Question10.Left),
    (res3, Question10.Left)]

tester (result, direction) = result == direction
passed = all tester test_vector