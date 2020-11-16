module Queries where

import AffineNecklaces
import Atoms
import Data.List (intersperse)
import Data.List.Split (chunksOf)
import Extension
import MarvellousSequences
import Necklaces
import Patterns
import PerfectRotations
import Phases
import System.Environment

-- Auxiliaries for display
showL :: [Int] -> String
showL = concatMap show

showLChunked :: Int -> [Int] -> String
showLChunked s l = unwords (map (concatMap show) (chunksOf s l))

printL :: [Int] -> IO ()
printL = putStrLn . showL

printLs :: [[Int]] -> IO ()
printLs = mapM_ printL

writeLs :: String -> [[Int]] -> IO ()
writeLs fileName = writeFile fileName . unlines . map showL

-- Queries

order2notinOrder3 =
  filter
    ( \seq2 ->
        not $
          any
            (any (\idx -> idx == 0 || idx == 3 * 2 ^ 2) . infixIndices seq2)
            nestedMarvellous233
    )
    nestedMarvellous223
  where
    nestedMarvellous233 = nestedMarvellous2 3 3
    nestedMarvellous223 = nestedMarvellous2 2 3

order2TimesInOrder3 =
  map
    ( \seq2 ->
        ( seq2,
          foldr
            (\seq (acc1, acc2) -> (acc1 + if elem 0 seq then 1 else 0, acc2 + if (3 * 2 ^ 2) `elem` seq then 1 else 0))
            (0, 0)
            ( map
                (\seq3 -> filter (\idx -> idx == 0 || idx == 3 * 2 ^ 2) (infixIndices seq2 seq3))
                nestedMarvellous233
            )
        )
    )
    nestedMarvellous223
  where
    nestedMarvellous233 = nestedMarvellous2 3 3
    nestedMarvellous223 = nestedMarvellous2 2 3

generateNestedPerfect2 n m = writeLs (concat ["../sequences/np-", show n, "-", show m, ".txt"]) $ recursiveNestedPerfect2 n m

generateNestedMarvellous2 n m = writeLs (concat ["../sequences/nm-", show n, "-", show m, ".txt"]) $ recursiveNestedMarvellous2 n m

generateNestedPerfectWithMatrix n d = writeLs (concat ["../sequences/np-mat-", show n, "-", show (2 ^ d), ".txt"]) $ allAffineNecklaces n d

atomPiecesOfPerfect :: Int -> Int -> [([Int], [Int])]
atomPiecesOfPerfect n d = map (\l -> (l, parseBinaryChunks (2 ^ d) l)) (allAffineNecklaces n d)

padShow :: Int -> Int -> String
padShow padd int = replicate (padd - length showedInt) ' ' ++ showedInt
  where
    showedInt = show int

showAtomPiecesOfPerfect :: Int -> Int -> [String]
showAtomPiecesOfPerfect n d =
  map
    (\(l, a) -> unlines [showLChunked (2 ^ d) l, unwords $ map (padShow (2 ^ d)) a])
    (atomPiecesOfPerfect n d)

writeAtomPiecesOfPerfect :: Int -> Int -> IO ()
writeAtomPiecesOfPerfect n d = writeFile fileName (unlines $ showAtomPiecesOfPerfect n d)
  where
    fileName = "../sequences/nested-perfect-" ++ show n ++ "," ++ show (2 ^ d) ++ ".txt"
