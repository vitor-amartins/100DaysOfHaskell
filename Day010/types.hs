data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * (r ^ 2)
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x' y') r) x y = Circle (Point (x' + x) (y' + y)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) x y = Rectangle (Point (x1 + x) (y1 + y)) (Point (x2 + x) (y2 + y))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String    
                     } deriving (Show)

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector x y z) = Vector (i+x) (j+y) (k+z)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector x y z) `vectMult` a = Vector (x*a) (y*a) (z*a)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector x y z) = i*x + j*y + k*z