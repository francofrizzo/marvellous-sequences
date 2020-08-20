module Secuencias where
import Data.List
import Data.List.Split
import Control.Monad (replicateM)
import Math.Combinat.Sets

-- Auxiliaries for display
d :: [Int] -> IO ()
d = putStrLn . concat . map show

dd :: [[Int]] -> IO ()
dd = mapM_ d

-- Basic methods for generating patterns

characters :: Int -> [Int]
characters k = [0 .. k - 1]

patterns :: Int -> Int -> [[Int]]
patterns k n = replicateM n (characters k)

toBinaryList :: Int -> [Int] -> [Int]
toBinaryList n indices = [ if elem i indices then 0 else 1 | i <- [0 .. n - 1] ]

balanced2Patterns :: Int -> [[Int]]
balanced2Patterns n | n `mod` 2 == 0 = map (toBinaryList n) (choose (n `div` 2) [0 .. n - 1])

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

-- De Bruijn sequences

isDeBruijn :: Int -> Int -> [Int] -> Bool
isDeBruijn k n seq = all (\p -> timesInfixOfNecklace p seq == 1) (patterns k n)

deBruijn :: Int -> Int -> [[Int]]
deBruijn k n = filter (isDeBruijn k n) (patterns k (k ^ n))

-- Filter for perfect and marvellous sequences
filterSequences :: Int -> Int -> Int -> (Int -> Int -> Int -> [Int] -> Bool) -> [[Int]]
filterSequences k n m pred = filter (pred k n m) (balanced2Patterns (m * (k ^ n)))

-- More efficient version for alphabet of size 2
filterSequences2 :: Int -> Int -> (Int -> Int -> [Int] -> Bool) -> [[Int]]
filterSequences2 n m pred = filter (pred n m) (balanced2Patterns (m * (2 ^ n)))

-- Perfect sequences
isPerfect :: Int -> Int -> Int -> [Int] -> Bool
isPerfect k n m seq = all (\p -> perfectCondition m (infixIndicesNecklace p seq)) (patterns k n)
  where perfectCondition m indices = length (nub (map (\x -> x `mod` m) indices)) == m

perfect :: Int -> Int -> Int -> [[Int]]
perfect k n m = filterSequences k n m isPerfect

perfect2 :: Int -> Int -> [[Int]]
perfect2 n m = filterSequences2 n m (isPerfect 2)

-- Marvellous sequences

isMarvellous :: Int -> Int -> Int -> [Int] -> Bool
isMarvellous k n m seq = all (\p -> timesInfixOfNecklace p seq == m) (patterns k n)

marvellous :: Int -> Int -> Int -> [[Int]]
marvellous k n m = filterSequences k n m isMarvellous

marvellous2 :: Int -> Int -> [[Int]]
marvellous2 n m = filterSequences2 n m (isMarvellous 2)

marvellousButNotPerfect :: Int -> Int -> Int -> [[Int]]
marvellousButNotPerfect k n m = filterSequences k n m (\k n m seq -> (isMarvellous k n m seq) && not (isPerfect k n m seq))

marvellousButNotPerfect2 :: Int -> Int -> [[Int]]
marvellousButNotPerfect2 n m = filterSequences2 n m (\n m seq -> (isMarvellous 2 n m seq) && not (isPerfect 2 n m seq))

-- Nested perfect sequences

isNestedPerfect :: Int -> Int -> Int -> [Int] -> Bool
isNestedPerfect k n m seq | n == 1 = (isPerfect k 1 m seq)
                          | n > 1  = (isPerfect k n m seq)
                                     && all (isNestedPerfect k (n - 1) m) (chunksOf ((length seq) `div` k) seq)

nestedPerfect :: Int -> Int -> Int -> [[Int]]
nestedPerfect k n m = filterSequences k n m isNestedPerfect

nestedPerfect2 :: Int -> Int -> [[Int]]
nestedPerfect2 n m = filterSequences2 n m (isNestedPerfect 2)

-- Nested marvellous sequences

isNestedMarvellous :: Int -> Int -> Int -> [Int] -> Bool
isNestedMarvellous k n m seq | n == 1 = (isMarvellous k 1 m seq)
                             | n > 1  = (isMarvellous k n m seq)
                                        && all (isNestedMarvellous k (n - 1) m) (chunksOf ((length seq) `div` k) seq)

nestedMarvellous :: Int -> Int -> Int -> [[Int]]
nestedMarvellous k n m = filterSequences k n m isNestedMarvellous

nestedMarvellous2 :: Int -> Int -> [[Int]]
nestedMarvellous2 n m = filterSequences2 n m (isNestedMarvellous 2)
