module Patterns where
import Control.Monad (replicateM)
import Data.List
import Math.Combinat.Sets
import Math.Combinat.Partitions

-- Basic methods for generating patterns

characters :: Int -> [Int]
characters k = [0 .. k - 1]

patterns :: Int -> Int -> [[Int]]
patterns k n = replicateM n (characters k)

-- Generates binary list of a given length, taking as parameters the 0 positions
toBinaryList :: Int -> [Int] -> [Int]
toBinaryList n indices = [ if i `elem` indices then 0 else 1 | i <- [0 .. n - 1] ]

balanced2Patterns :: Int -> [[Int]]
balanced2Patterns n | n `mod` 2 == 0 = map (toBinaryList n) (choose (n `div` 2) [0 .. n - 1])

-- Most efficient way of generating candidates for perfect & marvellous sequences
-- ... It does not work :(

candidate2PatternsSeeds :: Int -> Int -> [[Int]]
candidate2PatternsSeeds n m = map (\(x, y) -> concat $ zipWith (\v w -> [v,w]) x y)
  [(x, y) | x <- preSeeds, y <- preSeeds, length x == length y]
  where permutedPreSeeds = concatMap (nub . permutations) preSeeds
        preSeeds = map fromPartition $ partitions' (maxConsSymbolReps, maxSymbolAppearances) maxSymbolAppearances
        maxConsSymbolReps = n + m - 1
        maxSymbolAppearances = m * (2^(n - 1))

-- Generates binary list from 0 and 1 positions
-- List ends with 0 iff first parameter is True
toBinaryList' :: Bool -> [Int] -> [Int]
toBinaryList' lastIsZero = fst . foldr (\x (list, isZero) -> (replicate x (if isZero then 0 else 1) ++ list, not isZero)) ([], lastIsZero)

candidate2Patterns :: Int -> Int -> [[Int]]
candidate2Patterns n m = map (toBinaryList' True) (candidate2PatternsSeeds n m)


-- Another try of an efficient way to generate patterns
-- ... It does not work :(

limitedPatternsAux :: Int -> Int -> Int -> [(Int, Int, [Int])]
limitedPatternsAux lim k n | n == 1 = map (\c -> (1, c, [c])) (characters k)
                           | n > 1  = concatMap
                             (\(count, lastC, list) -> map (\c -> (if c == lastC then count + 1 else 1, c, c:list)) ((if count == lim then filter (\c2 -> c2 /= lastC) else id) (characters k)))
                             (limitedPatternsAux lim k (n - 1))

limitedPatterns :: Int -> Int -> Int -> [[Int]]
limitedPatterns lim k n = map (\ (_, _, l) -> l) $ limitedPatternsAux lim k n

candidatePatterns :: Int -> Int -> Int -> [[Int]]
candidatePatterns k n m = limitedPatterns (n + m - 1) k (m * 2^n)
