module Geometry.Sphere
( volume
, area
) where

volume :: (Num a) => a -> Float
volume r = (4.0 / 3.0) * pi * (r ^ 3)

area :: (Num a) => a -> Float
area r = 4 * pi * (r ^ 2)