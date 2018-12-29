factorial :: (Integral a) => a -> a
factorial n = product [1..n]

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n-1)

sumVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
sumVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

firstOf :: String -> String
firstOf "" = "Empty string!"
firstOf all@(x:_) = "The first letter of " ++ all ++ " is " ++ [x]

imc :: (RealFloat a) => a -> a -> a
imc height weight = weight / (height*height)

imcTell :: (RealFloat a) => a -> a -> String
imcTell height weight 
    | imcValue < 16.0 = "Magreza grave"
    | imcValue < 17.0 = "Magreza moderada"
    | imcValue < 18.5 = "Magreza leve"
    | imcValue < 25.0 = "Saudável"
    | imcValue < 30.0 = "Sobrepeso"
    | imcValue < 35.0 = "Obesidade Grau I"
    | imcValue < 40.0 = "Obesidade Grau II"
    | otherwise = "Obesidade Grau III"
    where imcValue = imc height weight