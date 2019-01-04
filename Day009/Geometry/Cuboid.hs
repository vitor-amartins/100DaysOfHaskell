module Geometry.Cuboid
( volume
, area
) where

volume :: (Num a) => a -> a -> a -> a
volume a b c = a * b * c

area :: (Num a) => a -> a -> a -> a
area a b c = 2 * ((a * (b + c)) + (b * c))