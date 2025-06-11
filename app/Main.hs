module Main where

import Matrix
import MatrixIO
import Data.Time.Clock (getCurrentTime, diffUTCTime)

main :: IO ()
main = do
    matrix1 <- readMatrixFromFile "input/matrix1.txt"
    matrix2 <- readMatrixFromFile "input/matrix2.txt"
    let (Matrix n1 m1 _) = matrix1
    let (Matrix n2 m2 _) = matrix2

    putStrLn $ "Choose multiplication method for matrices:"
    putStrLn "1. Simple multiplication"
    putStrLn "2. Fast multiplication"
    choice <- getLine

    case choice of
        "1" -> do
            putStrLn "You chose simple multiplication."
            putStrLn "Starting simple matrix multiplication..."
            start <- getCurrentTime
            let simpleMatrixMultiplication = multiplyMatrices matrix1 matrix2
            writeMatrixToFile "output/result1.txt" simpleMatrixMultiplication
            end <- getCurrentTime
            putStrLn $ "Simple multiplication time: " ++ show (diffUTCTime end start)
        "2" -> do
            putStrLn "You chose fast multiplication."
            putStrLn "Starting fast matrix multiplication..."
            start <- getCurrentTime
            let extendedMatrix1 = extendToTwoPowerSquare matrix1
                extendedMatrix2 = extendToTwoPowerSquare matrix2
                result = backToPreviousSize n1 m2 (fastMatrixMultiplication extendedMatrix1 extendedMatrix2)
            writeMatrixToFile "output/result2.txt" result
            end <- getCurrentTime
            putStrLn $ "Fast multiplication time: " ++ show (diffUTCTime end start)
        _   -> do 
            putStrLn "Invalid choice. Please choose 1 or 2."
            main
    