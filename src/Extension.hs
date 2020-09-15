module Extension where

import AffineNecklaces
import Control.Monad ((>=>))
import Data.List
import Data.List.Split
import Necklaces
import Patterns

patternsModulus :: Int -> Int -> Int -> [Int] -> [[Int]]
patternsModulus n m i seq = map (take n) (chunksOf m (rotateNecklace i seq))

missingPatternsModulus :: Int -> Int -> Int -> [Int] -> [[Int]]
missingPatternsModulus n m i seq = patterns 2 n \\ patternsModulus n m i seq

possibleTransformersModulus :: Int -> Int -> Int -> [Int] -> [[Int]]
possibleTransformersModulus n m i seq =
  map
    (zipWith (@+) (head (patternsModulus n m i seq)))
    (missingPatternsModulus n m i seq)

sequencesOverlap :: Eq a => [a] -> [a] -> Bool
sequencesOverlap s1 s2 = init s2 `isSuffixOf` s1

concatenations :: [Int] -> [[Int]] -> [[Int]]
concatenations s1 ss2 = [s1 ++ [last s2] | s2 <- ss2, sequencesOverlap s1 s2]

searchTransformers :: Int -> Int -> [Int] -> [[Int]]
searchTransformers n m seq =
  map (take m) $
    filter isCycle $
      foldr1 (>=>) (map step [1 .. m - 1]) =<< transformers 0
  where
    step i x = concatenations x (transformers i)
    transformers i = possibleTransformersModulus n m i seq
    isCycle xs = take (n - 1) xs `isSuffixOf` xs

(@@+) :: [Int] -> [Int] -> [Int]
(@@+) seq transformer = zipWith (@+) seq (cycle transformer)

searchExtensions :: Int -> Int -> [Int] -> [[Int]]
searchExtensions n m seq = map (seq @@+) (searchTransformers n m seq)
