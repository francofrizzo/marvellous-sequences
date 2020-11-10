module Atoms where

import Data.List
import Data.List.Split
import Data.Maybe
import MarvellousSequences
import Math.Combinat.Sets (countKSublists)
import Numeric (readInt)

atomsCount :: Int -> Integer
atomsCount m = countKSublists m (2 * m)

atoms :: Int -> [[Int]]
atoms = nestedMarvellous2 1

atomIndex :: Int -> [Int] -> Int
atomIndex m = fromJust . flip elemIndex (atoms m)

atomPairs :: Int -> [(Int, Int)]
atomPairs m = [(i + 1, j + 1) | i <- [0 .. length a - 1], j <- [0 .. length a - 1], isMarvellous 2 2 m ((a !! i) ++ (a !! j))]
  where
    a = atoms m

validAtomPairsProportion :: Int -> Float
validAtomPairsProportion m = (fromIntegral $ length (atomPairs m)) / (fromIntegral $ ((atomsCount m) ^ 2))

asAtoms :: Int -> [Int] -> [Int]
asAtoms m seq = map (atomIndex m) (chunksOf (2 * m) seq)

nestedMarvellousAsAtoms :: Int -> Int -> [[Int]]
nestedMarvellousAsAtoms n m = map (asAtoms m) (recursiveNestedMarvellous2 n m)

marvellousAutosimilar :: Int -> Int -> [[Int]]
marvellousAutosimilar n m
  | n == 1 = atoms m
  | otherwise = filter (isNestedMarvellous 2 n m) [x ++ x | x <- marvellousAutosimilar (n - 1) m]

marvellousWeaklyAutosimilar :: Int -> Int -> Int -> [[Int]]
marvellousWeaklyAutosimilar w n m
  | n == 1 = atoms m
  | w == 0 = marvellousAutosimilar n m
  | otherwise = filter (isMarvellous 2 n m) [x ++ y | x <- prevLev, y <- prevLev]
  where
    prevLev = marvellousWeaklyAutosimilar (w - 1) (n - 1) m

--

perfectAtoms :: Int -> [[Int]]
perfectAtoms = nestedPerfect2 1

perfectAtomIndex :: Int -> [Int] -> Int
perfectAtomIndex m = fromJust . flip elemIndex (perfectAtoms m)

asPerfectAtoms :: Int -> [Int] -> [Int]
asPerfectAtoms m seq = map (perfectAtomIndex m) (chunksOf (2 * m) seq)

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
