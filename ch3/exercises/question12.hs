module Question12 where

import Data.List

data Direction = Left | Right | Straight deriving (Eq, Show)

-- Note that Point2 and Vec2 while structurally identical
-- are semantically different. Point2 is simple a point on
-- the two dimensional cartesian plane. Vec2 is a vector in R2
-- anchored at zero. Mathematically, taking the determinant
-- of two points makes no sense, so here, we only take the
-- determinant of two vectors.
data Point2 = Point2 Integer Integer deriving (Show, Eq)
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

-- function that determines if this_vec is to the left or
-- right of ref_vec
direction_relative_to this_vec ref_vec
    | dir < 0  = Question12.Left
    | dir > 0  = Question12.Right
    | dir == 0 = Question12.Straight
    where
        dir = det2 this_vec ref_vec

-- a function that takes two points, p1 and p2, with respect to
-- a pivot point on a cartesian plane, and determines whether p2
-- is rotated to the right or left of p1 about the pivot
angle_relative_to pivot_point p1 p2
    | dir == Question12.Right    = LT
    | dir == Question12.Left     = GT
    | dir == Question12.Straight = EQ
    where dir = vec1 `direction_relative_to` vec2
          vec1 = makeVec pivot_point p1
          vec2 = makeVec pivot_point p2

-- take a list of points and find the geometrically lowest point
-- by y coordinate in that list
lowest_point (point:[])          = point
lowest_point (this_point:rest)   = lower_point this_point (lowest_point rest)
     where lower_point p1@(Point2 _ y1) p2@(Point2 _ y2)
            | y1 < y2   = p1
            | y2 < y1   = p2
            | otherwise = p1

-- given a path composed of three points, p1, p2, and p3, we
-- want to know if the path curves left, right, or goes straight
path_direction p1 p2 p3 =
    let vec1 = makeVec p1 p2 in
    let vec2 = makeVec p2 p3 in
    vec2 `direction_relative_to` vec1

-- finally! What we've been waiting for!!

-- we must first add two points to the convex hull to start off
convex_hull points_on_hull@[] unevaluated_points@(p1:p2:rem) = 
    convex_hull [p2, p1] rem

-- no more unevaluated points is the stopping condition
convex_hull points_on_hull [] = points_on_hull

-- then we run the convex hull algorithm at steady state
convex_hull points_on_hull unevaluated_points = 
    if direction == Question12.Right
        then convex_hull (tail points_on_hull) unevaluated_points
        else convex_hull ([p3] ++ points_on_hull) (tail unevaluated_points)
    where
        direction = path_direction p1 p2 p3
        p1        = head $ tail points_on_hull
        p2        = head points_on_hull
        p3        = head unevaluated_points

-- TESTS --
-- a diagram with the following points can be 
-- found in resources/question12.svg
set1 = [
    Point2 3 0, Point2 2 1, Point2 1 2,
    Point2 1 3, Point2 2 3, Point2 2 4,
    Point2 2 5, Point2 1 6, Point2 3 7,
    Point2 3 4, Point2 3 2]

starting_point = lowest_point set1
angle_relative_to_starting_point = angle_relative_to starting_point
sorted_points  = sortBy angle_relative_to_starting_point set1
hull = convex_hull [] sorted_points

main = putStrLn $ show $ [(x,y) | (Point2 x y) <- hull]