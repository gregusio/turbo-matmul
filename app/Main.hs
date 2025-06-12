module Main where


import Matrix
import MatrixIO
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Matrix


main :: IO ()
main = do
    matrix1 <- readMatrixFromFile "input/matrix1.txt"
    matrix2 <- readMatrixFromFile "input/matrix2.txt"
    let (Matrix n1 _ a) = matrix1
    let (Matrix _ m2 b) = matrix2

    putStrLn $ "Choose multiplication method for matrices:"
    putStrLn "1. Simple multiplication"
    putStrLn "2. Fast multiplication"
    putStrLn "3. Data.Matrix library standard multiplication (1)"
    putStrLn "4. Data.Matrix library standard multiplication (2)"
    choice <- getLine

    case choice of
        "1" -> do
            putStrLn "You chose simple multiplication."
            putStrLn "Starting simple matrix multiplication..."
            start <- getCurrentTime
            let simpleMatrixMultiplication = simpleMultiply matrix1 matrix2
            writeMatrixToFile "output/result1.txt" simpleMatrixMultiplication
            end <- getCurrentTime
            putStrLn $ "Simple multiplication time: " ++ show (diffUTCTime end start)
        "2" -> do
            putStrLn "You chose fast multiplication."
            putStrLn "Starting fast matrix multiplication..."
            start <- getCurrentTime
            let result = fastMultiply matrix1 matrix2
            writeMatrixToFile "output/result2.txt" result
            end <- getCurrentTime
            putStrLn $ "Fast multiplication time: " ++ show (diffUTCTime end start)
        "3" -> do
            putStrLn "You chose to compare with Data.Matrix library."
            putStrLn "Starting Data.Matrix (1) multiplication..."
            start <- getCurrentTime
            let matrix1' = fromLists a
                matrix2' = fromLists b
                result1 = matrix1' `multStd` matrix2'
                matrix_result1 = Matrix n1 m2 (toLists result1)
            writeMatrixToFile "output/result3.txt" matrix_result1
            end <- getCurrentTime
            putStrLn $ "Data.Matrix multiplication time: " ++ show (diffUTCTime end start)
        "4" -> do
            putStrLn "You chose to compare with Data.Matrix library (2)."
            putStrLn "Starting Data.Matrix multiplication..."
            start <- getCurrentTime
            let matrix1' = fromLists a
                matrix2' = fromLists b
                result2 = matrix1' `multStd2` matrix2'
                matrix_result2 = Matrix n1 m2 (toLists result2)
            writeMatrixToFile "output/result3.txt" matrix_result2
            end <- getCurrentTime
            putStrLn $ "Data.Matrix multiplication time: " ++ show (diffUTCTime end start)
        _   -> do 
            putStrLn "Invalid choice. Please choose 1 or 2."
            main

    