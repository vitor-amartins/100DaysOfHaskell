double :: Num t => t -> t
double x = x + x

square :: Num t => t -> t
square x = x * x

squareSum :: Num t => t -> t -> t
squareSum x y = square x + square y

concat :: [t] -> [t] -> [t]
concat list1 list2 = list1 ++ list2

insertFirst :: t -> [t] -> [t]
insertFirst item list = item:list

getElementAt :: [t] -> Int -> t
getElementAt list index = list !! index


---- List functions ----
-- head: First element of list
-- tail: Rest of list
-- last: Last element of list
-- init: List without last element
-- length: Length of list
-- null: If list is empty
-- reverse: Reverse list
-- take: Get n first elements of list
-- drop: Remove n first elements of list
-- maximum: The biggest element
-- minimum: The smallest element
-- sum: The sum of the elements
-- product: The product of the elements
-- `elem`: If a element is on the list 

reverseConcat :: [t] ->  [t] -> [t]
reverseConcat x y = reverse x ++ reverse y

concatReverse :: [t] ->  [t] -> [t]
concatReverse x y = reverse (x ++ y)

removeUppercase :: String -> String
removeUppercase str = [c | c <- str, not (c `elem` ['A'..'Z'])]

removeNonCharacter :: String -> String
removeNonCharacter str = [c | c <- str, c `elem` ['A'..'Z'] || c `elem` ['a'..'z']]

dist :: (Double, Double) -> Double
dist pair = sqrt (squareSum (fst pair) (snd pair))

sumPair :: Num t => (t, t) -> t
sumPair pair = fst pair + snd pair

-- sumEach :: Num t => [t] -> [t] -> [t]
-- sumEach list1 list2 = (zip list1 list2)