sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

-- foo a = bar b a
-- foo = bar b

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

foldl' f acc xs = f acc (foldl1 f xs)

-- foldl and foldr work fine with empty lists!

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- sum $ filter (>10) $ map (*2) [2..10]

negateAll :: (Num a) => [a] -> [a]
negateAll xs = map (negate . abs) xs

oddSquareSum :: Integer
-- oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]