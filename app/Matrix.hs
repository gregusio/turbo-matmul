module Matrix (
    Matrix(..),
    extendToTwoPowerSquare,
    divideMatrixToQuadrants,
    addMatrices,
    combineQuadrants,
    multiplyMatrices,
    fastMatrixMultiplication,
    backToPreviousSize
) where


import Data.List (transpose)
import Control.Parallel.Strategies (rpar, rseq, runEval)
import Control.DeepSeq (NFData, rnf, force)


data Matrix = Matrix Int Int [[Double]]
    deriving (Show)


instance NFData Matrix where
    rnf (Matrix n m xs) = rnf n `seq` rnf m `seq` rnf xs 


extendToTwoPowerSquare :: Matrix -> Matrix
extendToTwoPowerSquare (Matrix n m matrix) =
    let size = 2 ^ (ceiling $ logBase 2 (fromIntegral $ max n m))
        padRow row = take size (row ++ repeat 0.0)
        paddedRows = map padRow matrix
        missingRows = size - n
    in Matrix size size (paddedRows ++ replicate missingRows (replicate size 0.0))


backToPreviousSize :: Int -> Int -> Matrix -> Matrix
backToPreviousSize originalN originalM (Matrix n m matrix) =
    let trimmedRows = take originalN matrix
        trimmedMatrix = map (take originalM) trimmedRows
    in Matrix originalN originalM trimmedMatrix


divideMatrixToQuadrants :: Matrix -> (Matrix, Matrix, Matrix, Matrix)
divideMatrixToQuadrants (Matrix n m matrix) = 
    let halfN = n `div` 2
        halfM = m `div` 2
        topLeft = take halfN $ map (take halfM) matrix
        topRight = take halfN $ map (drop halfM) matrix
        bottomLeft = drop halfN $ map (take halfM) matrix
        bottomRight = drop halfN $ map (drop halfM) matrix
    in (Matrix halfN halfM topLeft, Matrix halfN halfM topRight, Matrix halfN halfM bottomLeft, Matrix halfN halfM bottomRight)


addMatrices :: Matrix -> Matrix -> Matrix
addMatrices (Matrix n1 m1 a) (Matrix n2 m2 b) 
    | n1 /= n2 || m1 /= m2 = error "Matrices must have the same dimensions for addition"
    | otherwise = 
        if n1 == 0 || m1 == 0 then Matrix n1 m1 []
        else Matrix n1 m1 (zipWith (zipWith (+)) a b)



combineQuadrants :: Matrix -> Matrix -> Matrix -> Matrix -> Matrix
combineQuadrants (Matrix _ _ c11) (Matrix _ _ c12) (Matrix _ _ c21) (Matrix _ _ c22) = 
    let top = zipWith (++) c11 c12
        bottom = zipWith (++) c21 c22
    in Matrix (length top) (length (head top)) (top ++ bottom)


multiplyMatrices :: Matrix -> Matrix -> Matrix
multiplyMatrices (Matrix n1 m1 a) (Matrix n2 m2 b)
    | m1 /= n2 = error "Incompatible matrix dimensions"
    | otherwise = Matrix n1 m2 (multiplyMatrices' a b)
    where
        multiplyMatrices' :: [[Double]] -> [[Double]] -> [[Double]]
        multiplyMatrices' a b = [[sum $ zipWith (*) ar bc | bc <- transpose b] | ar <- a]


fastMatrixMultiplication :: Matrix -> Matrix -> Matrix
fastMatrixMultiplication (Matrix n1 m1 a) (Matrix n2 m2 b)
    | m1 /= n2 = error "Incompatible matrix dimensions"
    | otherwise = runEval $ do
        let (tl1, tr1, bl1, br1) = divideMatrixToQuadrants (Matrix n1 m1 a)
            (tl2, tr2, bl2, br2) = divideMatrixToQuadrants (Matrix n2 m2 b)
        c11 <- rpar (force(addMatrices (multiplyMatrices tl1 tl2) (multiplyMatrices tr1 bl2)))
        c12 <- rpar (force(addMatrices (multiplyMatrices tl1 tr2) (multiplyMatrices tr1 br2)))
        c21 <- rpar (force(addMatrices (multiplyMatrices bl1 tl2) (multiplyMatrices br1 bl2)))
        c22 <- rpar (force(addMatrices (multiplyMatrices bl1 tr2) (multiplyMatrices br1 br2)))
        rseq c11
        rseq c12
        rseq c21
        rseq c22
        return (combineQuadrants c11 c12 c21 c22)
