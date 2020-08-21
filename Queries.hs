module Queries where
import Patterns
import Necklaces
import MarvellousSequences
import AffineNecklaces
import qualified Data.Vector as Vec

-- Auxiliaries for display
showL :: [Int] -> String
showL = concat . map show

printL :: [Int] -> IO ()
printL = putStrLn . showL

printLs :: [[Int]] -> IO ()
printLs = mapM_ printL

writeLs :: String -> [[Int]] -> IO ()
writeLs fileName = writeFile fileName . unlines . map showL

-- Queries

order2notinOrder3 = filter
                      (\seq2 ->
                        not $ any
                          (\seq3 -> any (\idx -> idx == 0 || idx == 3 * 2^2) (infixIndices seq2 seq3))
                          nestedMarvellous233
                      )
                      nestedMarvellous223
                    where nestedMarvellous233 = nestedMarvellous2 3 3
                          nestedMarvellous223 = nestedMarvellous2 2 3

order2TimesInOrder3 = map
                      (\seq2 -> (
                        seq2,
                        foldr
                          (\seq (acc1, acc2) -> (acc1 + if elem 0 seq then 1 else 0, acc2 + if elem (3 * 2^2) seq then 1 else 0)) (0,0)
                          (map
                            (\seq3 -> filter (\idx -> idx == 0 || idx == 3 * 2^2) (infixIndices seq2 seq3))
                            nestedMarvellous233)
                      ) )
                      nestedMarvellous223
                    where nestedMarvellous233 = nestedMarvellous2 3 3
                          nestedMarvellous223 = nestedMarvellous2 2 3

generateNestedPerfect n m = writeLs (concat ["sequences/np-", show n, "-", show m, ".txt"]) $ recursiveNestedPerfect2 n m
generateNestedMarvellous n m = writeLs (concat ["sequences/nm-", show n, "-", show m, ".txt"]) $ recursiveNestedMarvellous2 n m

generateNestedPerfectWithMatrix n d = writeLs (concat ["sequences/np-mat-", show n, "-", show (2^d), ".txt"]) $ map Vec.toList $ allAffineNecklaces n d