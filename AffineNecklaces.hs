module AffineNecklaces where
import qualified Data.Vector as Vec
import Data.Matrix hiding ((!))
import Patterns

-- Here I follow the notation used in the paper
-- The n parameter here is called k -the length of the subpatterns to be found in sequences
-- The m parameter -the amount of times each of them shoud appear- is assumed to be a power of 2, 2^d

-- Variant of the Pascal triangle matrix
baseMatrix :: Int -> Matrix Int
baseMatrix d = foldl
                 (\prevMat x -> joinBlocks (prevMat, prevMat, zero (2^(x-1)) (2^(x-1)), prevMat))
                 (matrix 1 1 $ const 1)
                 [1..d]

-- Shifts a matix column forwards a certain amount of positions
shiftCol :: Int -> Int -> Matrix Int -> Matrix Int
shiftCol j k mat = mapCol shiftCol j mat
                     where shiftCol i _ = currCol Vec.! ((i - k - 1) `mod` size)
                           currCol = getCol j mat
                           size = nrows mat

-- Shifts consecutive columns of a matrix
shiftCols :: [Int] -> Matrix Int -> Matrix Int
shiftCols ks = foldr (.) id (zipWith shiftCol [1 ..] ks)

-- All the possible shifts for the base matrix considered in the paper
possibleShifts :: Int -> [[Int]]
possibleShifts m = foldr (\_ recShifts -> concatMap (\ (x : xs) -> [x : x : xs, x + 1 : x : xs]) recShifts) [[0]] [1..m-1]

-- The base matrix applying all possible shifts
possibleMatrices :: Int -> [Matrix Int]
possibleMatrices d = map (`shiftCols` base) (possibleShifts m)
                     where m = 2^d
                           base = baseMatrix d

-- All possible binary words for a certain length
possibleWords :: Int -> [Vec.Vector Int]
possibleWords = map Vec.fromList . patterns 2

-- k and m are parameters with m = 2^d for some d
-- mat is of size m
-- z is a word of length m = 2^d
affineNecklace :: Int -> Int -> Matrix Int -> Vec.Vector Int -> Vec.Vector Int
affineNecklace k d mat z = foldr1 (Vec.++) $ map (getMatrixAsVector . multMod2 mat . colVector) words'
                           where multMod2 m1 m2 = mapPos (\_ x -> x `mod` 2) $ multStd m1 m2
                                 words' = map (Vec.zipWith (\x y -> (x + y) `mod` 2) z) words
                                 words = take (2^k) (possibleWords m)
                                 m = 2^d

-- Generate all the possible affine necklace for a certain (k, d) pair
allAffineNecklacesVec :: Int -> Int -> [Vec.Vector Int]
allAffineNecklacesVec k d = [affineNecklace k d mat z |
                               mat <- possibleMatrices d,
                               z <- possibleWords m
                            ]  
                            where m = 2^d

allAffineNecklaces :: Int -> Int -> [[Int]]
allAffineNecklaces k d = map Vec.toList $ allAffineNecklacesVec k d
