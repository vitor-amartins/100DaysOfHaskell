import Data.List
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

onlyGreaterThan :: (Ord a) => [a] -> a -> [a]
onlyGreaterThan xs a = filter (> a) xs
