module Necklaces where
import Data.List

-- Checking infix of necklace

isInfixOfNecklace :: (Eq a) => [a] -> [a] -> Bool
isInfixOfNecklace [] ys = True
isInfixOfNecklace xs ys = isInfixOf xs (ys ++ take (length xs - 1) ys)

timesInfixOf :: (Eq a) => [a] -> [a] -> Int
timesInfixOf xs [] = 0
timesInfixOf xs (y:ys) = (if isPrefixOf xs (y:ys) then 1 else 0) + (timesInfixOf xs ys)

timesInfixOfNecklace :: (Eq a) => [a] -> [a] -> Int
timesInfixOfNecklace [] ys = 0
timesInfixOfNecklace xs ys = timesInfixOf xs (ys ++ take (length xs - 1) ys)

infixIndicesAux :: (Eq a) => Int -> [a] -> [a] -> [Int]
infixIndicesAux n xs [] = []
infixIndicesAux n xs (y:ys) | isPrefixOf xs (y:ys) = n : (infixIndicesAux (n + 1) xs ys)
                            | otherwise          = (infixIndicesAux (n + 1) xs ys)

infixIndices :: (Eq a) => [a] -> [a] -> [Int]
infixIndices = infixIndicesAux 0

infixIndicesNecklace :: (Eq a) => [a] -> [a] -> [Int]
infixIndicesNecklace [] ys = []
infixIndicesNecklace xs ys = infixIndices xs (ys ++ take (length xs - 1) ys)

-- Finding infixes of necklace

infixesOf :: Int -> [a] -> [[a]]
infixesOf n (x:xs) | length (x:xs) < n = []
                 | otherwise         = (take n (x:xs)) : (infixesOf n xs)

infixesOfNecklace :: Int -> [a] -> [[a]]
infixesOfNecklace n xs = infixesOf n (xs ++ (take (n - 1) xs))
