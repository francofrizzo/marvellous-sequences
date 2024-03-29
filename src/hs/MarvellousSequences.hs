module MarvellousSequences where

import Data.List
import Data.List.Split
import Math.Combinat.Permutations (isPermutation)
import Necklaces
import Patterns

-- De Bruijn sequences

isDeBruijn :: Int -> Int -> [Int] -> Bool
isDeBruijn k n seq = all (\p -> timesInfixOfNecklace p seq == 1) (patterns k n)

-- More efficient version for alphabet of size 2
deBruijn :: Int -> Int -> [[Int]]
deBruijn k n = filter (isDeBruijn k n) (patterns k (k ^ n))

deBruijn2 :: Int -> [[Int]]
deBruijn2 n = filter (isDeBruijn 2 n) (balanced2Patterns (2 ^ n))

-- Filter for perfect and marvellous sequences
filterSequences :: Int -> Int -> Int -> (Int -> Int -> Int -> [Int] -> Bool) -> [[Int]]
filterSequences k n m pred = filter (pred k n m) (patterns k (m * (k ^ n)))

-- More efficient version for alphabet of size 2
filterSequences2 :: Int -> Int -> (Int -> Int -> [Int] -> Bool) -> [[Int]]
filterSequences2 n m pred = filter (pred n m) (balanced2Patterns (m * (2 ^ n)))

-- Perfect sequences
isPerfect :: Int -> Int -> Int -> [Int] -> Bool
isPerfect k n m seq = all (\p -> perfectCondition m (infixIndicesNecklace p seq)) (patterns k n)
  where
    perfectCondition m indices = isPermutation (map (\x -> x `mod` m + 1) indices)

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
marvellousButNotPerfect k n m = filterSequences k n m (\k n m seq -> isMarvellous k n m seq && not (isPerfect k n m seq))

marvellousButNotPerfect2 :: Int -> Int -> [[Int]]
marvellousButNotPerfect2 n m = filterSequences2 n m (\n m seq -> isMarvellous 2 n m seq && not (isPerfect 2 n m seq))

-- Nested perfect sequences

isNestedPerfect :: Int -> Int -> Int -> [Int] -> Bool
isNestedPerfect k n m seq
  | n == 1 = isPerfect k 1 m seq
  | n > 1 =
    isPerfect k n m seq
      && all (isNestedPerfect k (n - 1) m) (chunksOf (length seq `div` k) seq)

nestedPerfect :: Int -> Int -> Int -> [[Int]]
nestedPerfect k n m = filterSequences k n m isNestedPerfect

nestedPerfect2 :: Int -> Int -> [[Int]]
nestedPerfect2 n m = filterSequences2 n m (isNestedPerfect 2)

recursiveNestedPerfect2 :: Int -> Int -> [[Int]]
recursiveNestedPerfect2 n m
  | n == 1 = filterSequences2 n m (isNestedPerfect 2)
  | otherwise = filter (isNestedPerfect 2 n m) [s1 ++ s2 | s1 <- prevOrderSequences, s2 <- prevOrderSequences]
  where
    prevOrderSequences = recursiveNestedPerfect2 (n - 1) m

-- Nested marvellous sequences

isNestedMarvellous :: Int -> Int -> Int -> [Int] -> Bool
isNestedMarvellous k n m seq
  | n == 1 = isMarvellous k 1 m seq
  | n > 1 =
    isMarvellous k n m seq
      && all (isNestedMarvellous k (n - 1) m) (chunksOf (length seq `div` k) seq)

nestedMarvellous :: Int -> Int -> Int -> [[Int]]
nestedMarvellous k n m = filterSequences k n m isNestedMarvellous

nestedMarvellous2 :: Int -> Int -> [[Int]]
nestedMarvellous2 n m = filterSequences2 n m (isNestedMarvellous 2)

recursiveNestedMarvellous2 :: Int -> Int -> [[Int]]
recursiveNestedMarvellous2 n m
  | n == 1 = filterSequences2 n m (isNestedMarvellous 2)
  | otherwise = filter (isNestedMarvellous 2 n m) [s1 ++ s2 | s1 <- prevOrderSequences, s2 <- prevOrderSequences]
  where
    prevOrderSequences = recursiveNestedMarvellous2 (n - 1) m

-- Recursive nested marvellous with DB atoms
recursiveNestedMarvellousDB2 :: Int -> Int -> [[Int]]
recursiveNestedMarvellousDB2 n d
  | n == 1 = deBruijn2 (d + 1)
  | otherwise = filter (isMarvellous 2 n (2 ^ d)) [s1 ++ s2 | s1 <- prevOrderSequences, s2 <- prevOrderSequences]
  where
    prevOrderSequences = recursiveNestedMarvellousDB2 (n - 1) d

whiteRecursiveNestedMarvellous2 :: Int -> Int -> [[Int]]
whiteRecursiveNestedMarvellous2 n m
  | n == 1 = filter (\s -> head s == 0) $ filterSequences2 n m (isNestedMarvellous 2)
  | otherwise = filter (isMarvellous 2 n m) [s1 ++ s2 | s1 <- prevOrderSequences, s2 <- prevOrderSequences]
  where
    prevOrderSequences = whiteRecursiveNestedMarvellous2 (n - 1) m

-- Count patterns
countPatterns :: Int -> Int -> [Int] -> [([Int], Int)]
countPatterns k n seq = map (\p -> (p, timesInfixOfNecklace p seq)) (patterns k n)
