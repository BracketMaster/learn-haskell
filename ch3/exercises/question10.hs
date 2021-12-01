module Question10 where

data Direction = Left | Right | Straight deriving (Show)

data Vec2   = Vec2 Integer Integer deriving (Show)
data Ray2   = Ray2 Vec2 Vec2 deriving (Show)

ray2_to_vec2 
    (Ray2 (Vec2 i1 j1) (Vec2 i2 j2)) = 
        (Vec2 (i2 - i1) (j2 - j1))

-- A positive determinant means that first vector argument
-- is to the right of the second vector argument.
-- A negative determinant means that first vector argument
-- is to the left of the second vector argument.
det2 (Vec2 i1 j1) (Vec2 i2 j2) = i1*j2 - i2*j1

(direction_relative_to) this_vec ref_vec
    | dir < 0  = Question10.Left
    | dir > 0  = Question10.Right
    | dir == 0 = Question10.Straight
    where
        dir = det2 this_vec ref_vec

-- the following points correspond to the
-- image in resources/question10.svg
a = Vec2 1 0
b = Vec2 0 1
c = Vec2 2 3
d = Vec2 4 2
e = Vec2 3 1
f = Vec2 5 1

r0 = Ray2 a b
r1 = Ray2 a c
r2 = Ray2 a d
r3 = Ray2 a e
r4 = Ray2 a f
r5 = Ray2 c d
r6 = Ray2 c b

rays    = [r0, r1, r2, r3, r4, r5, r6]
vectors = [ray2_to_vec2 ray | ray <- rays]