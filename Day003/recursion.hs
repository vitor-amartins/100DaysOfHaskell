replicate' :: (Num n, Eq n) => n -> a -> [a]
replicate' 0 _ = []
replicate' n a = a:(replicate' (n-1) a)

take' :: (Num n, Eq n) => n -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x:(take' (n-1) xs)

contain :: (Num n, Eq n) => n -> [n] -> Bool
contain _ [] = False
contain n (x:xs)
    | n == x = True
    | otherwise = contain n xs

count :: (Num n, Eq n, Integral a) => n -> [n] -> a
count _ [] = 0
count n (x:xs)
    | n == x = 1 + count n xs
    | otherwise = count n xs

reverse' :: [t] -> [t]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [a | a <- xs, a <= x] ++ [x] ++ quicksort [a | a <- xs, a > x]
