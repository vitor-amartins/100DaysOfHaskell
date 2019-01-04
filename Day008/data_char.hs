import Data.Char

encode' :: Int -> String -> String
encode' 0 str = str
encode' _ [] = []
encode' n (x:xs) =  (chr ((ord x) + n)):encode' n xs

decode' :: Int -> String -> String
decode' n str = encode' (negate n) str