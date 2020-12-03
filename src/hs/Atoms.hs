module Atoms where

import Data.List
import Data.List.Split
import Data.Maybe
import MarvellousSequences
import Math.Combinat.Sets (countKSublists)

atoms2Count :: Int -> Integer
atoms2Count m = countKSublists m (2 * m)

atoms :: Int -> Int -> [[Int]]
atoms k = nestedMarvellous k 1

atomIndex :: Int -> Int -> [Int] -> Int
atomIndex k m = fromJust . flip elemIndex (atoms k m)

atom2Pairs :: Int -> [(Int, Int)]
atom2Pairs m = [(i + 1, j + 1) | i <- [0 .. length a - 1], j <- [0 .. length a - 1], isMarvellous 2 2 m ((a !! i) ++ (a !! j))]
  where
    a = atoms 2 m

validAtom2PairsProportion :: Int -> Float
validAtom2PairsProportion m = (fromIntegral $ length (atom2Pairs m)) / (fromIntegral $ ((atoms2Count m) ^ 2))

asAtoms :: Int -> Int -> [Int] -> [Int]
asAtoms k m seq = map (atomIndex k m) (chunksOf (k * m) seq)

nestedMarvellous2AsAtoms :: Int -> Int -> [[Int]]
nestedMarvellous2AsAtoms n m = map (asAtoms 2 m) (recursiveNestedMarvellous2 n m)

marvellousAutosimilar :: Int -> Int -> Int -> [[Int]]
marvellousAutosimilar k n m
  | n == 1 = atoms k m
  | otherwise = filter (isNestedMarvellous k n m) [x ++ x ++ x | x <- marvellousAutosimilar k (n - 1) m]

marvellousWeaklyAutosimilar :: Int -> Int -> Int -> Int -> [[Int]]
marvellousWeaklyAutosimilar k w n m
  | n == 1 = atoms k m
  | w == 0 = marvellousAutosimilar k n m
  | otherwise = filter (isMarvellous k n m) [x ++ y ++ z | x <- prevLev, y <- prevLev, z <- prevLev]
  where
    prevLev = marvellousWeaklyAutosimilar k (w - 1) (n - 1) m

--

perfectAtoms :: Int -> Int -> [[Int]]
perfectAtoms k = nestedPerfect k 1

perfectAtomIndex :: Int -> Int -> [Int] -> Int
perfectAtomIndex k m = fromJust . flip elemIndex (perfectAtoms k m)

asPerfectAtoms :: Int -> Int -> [Int] -> [Int]
asPerfectAtoms k m seq = map (perfectAtomIndex k m) (chunksOf (k * m) seq)

--

parseBinary :: [Int] -> Int
parseBinary = foldl' (\acc x -> acc * 2 + x) 0

parseBinaryChunks :: Int -> [Int] -> [Int]
parseBinaryChunks size list = map parseBinary (chunksOf size list)

showBinaryChunks :: Int -> [Int] -> String
showBinaryChunks size list = unlines [showLChunked size list, unwords $ map (padShow size) (parseBinaryChunks size list)]
  where
    showLChunked size list = unwords (map (concatMap show) (chunksOf size list))
    padShow padd int = replicate (padd - length (show int)) ' ' ++ show int

putBinaryChunks :: Int -> [[Int]] -> IO ()
putBinaryChunks size lists = putStr $ unlines $ map (showBinaryChunks size) lists
