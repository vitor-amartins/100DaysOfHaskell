module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where

sphereVolume :: (Num a) => a -> Float
sphereVolume r = (4.0 / 3.0) * pi * (r ^ 3)

sphereArea :: (Num a) => a -> Float
sphereArea r = 4 * pi * (r ^ 2)

cubeVolume :: (Num a) => a -> a
cubeVolume a = (a ^ 3)

cubeArea :: (Num a) => a -> a
cubeArea a = 6 * (a ^ 2)

cuboidVolume :: (Num a) => a -> a -> a -> a
cuboidVolume a b c = a * b * c

cuboidArea :: (Num a) => a -> a -> a -> a
cuboidArea a b c = 2 * ((a * (b + c)) + (b * c))