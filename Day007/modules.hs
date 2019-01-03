import Data.List

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [a] = [a]
intersperse' i (x:xs) = [x] ++ [i] ++ intersperse' i xs

intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ [x] = x
intercalate' i (x:xs) = x ++ i ++ intercalate' i xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' [x] = x
concat' (x:xs) = x ++ concat' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs)
    | x = and' xs
    | otherwise = False

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs)
    | x = True
    | otherwise = or' xs

myIterate f x = x:myIterate f (f x)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x:takeWhile' f xs
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
    | f x = dropWhile' f xs
    | otherwise = x:xs
