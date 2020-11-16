module PerfectRotations where

import AffineNecklaces (allAffineNecklaces)
import Data.List (nub)
import MarvellousSequences (isNestedMarvellous, isNestedPerfect)
import Necklaces (rotateNecklace)

rotateFirst :: [Int] -> Int -> Int -> [Int]
rotateFirst seq i n = rotateNecklace n (take i seq) ++ drop i seq

rotateLast :: [Int] -> Int -> Int -> [Int]
rotateLast seq i n = take i seq ++ rotateNecklace n (drop i seq)

perfectRotations :: Int -> Int -> [[[Int]]]
perfectRotations n d = map (\seq -> map (rotateLast seq halfSize) rotationIndices) nestedPerfect
  where
    nestedPerfect = allAffineNecklaces n d
    halfSize = 2 ^ (n + d -1)
    rotationIndices = [1]

filteredPerfectRotations :: Int -> Int -> [[Int]]
filteredPerfectRotations n d = nub $ filter (\s -> isNestedMarvellous 2 n (2 ^ d) s && not (isNestedPerfect 2 n (2 ^ d) s)) (concat $ perfectRotations n d)
