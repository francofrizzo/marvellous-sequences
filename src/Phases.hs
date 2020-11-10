module Phases where

import Data.List (elemIndex, nub)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

getPhases :: Int -> [Int] -> [[Int]]
getPhases m l = map (\j -> map fst $ filter (\(_, i) -> i `mod` m == j) lWithIndices) [0 .. m -1]
  where
    lWithIndices = zip l [0 .. length l - 1]

phaseType :: [Int] -> Int
phaseType p = head $ filter (\x -> 1 == length (nub $ chunksOf (2 ^ x) p)) [0 ..]

cannonicalTreeLeaves :: [Int]
cannonicalTreeLeaves = [0, 15, 10, 5, 3, 12, 9, 6, 4, 11, 14, 1, 7, 8, 13, 2]

tree :: [Int] -> [[Bool]]
tree s =
  [ [p 0 > p 8],
    [p 0 > p 4, p 8 > p 12],
    [p 0 > p 2, p 4 > p 6, p 8 > p 10, p 12 > p 14],
    [p 0 > p 1, p 2 > p 3, p 4 > p 5, p 6 > p 7, p 8 > p 9, p 10 > p 11, p 12 > p 13, p 14 > p 15]
  ]
  where
    p i = fromJust (elemIndex (leaf i) s) -- leaf position
    leaf i = cannonicalTreeLeaves !! i
