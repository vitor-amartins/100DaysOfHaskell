double :: (Num a) => a -> a
double = (*2)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyNTimes :: (Num b, Eq b) => (a-> a) -> a -> b -> a
applyNTimes f x 1 = f x
applyNTimes f x n = f (applyNTimes f x (n-1)) 

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort' (filter' (<= x) xs) ++ [x] ++ quicksort' (filter' (> x) xs)

fisrtWord :: String -> String
fisrtWord [] = []
fisrtWord str = takeWhile (/= ' ') str

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter (>15) (map length (map chain [1..100])))