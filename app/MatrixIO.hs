module MatrixIO (
    readMatrixFromFile,
    writeMatrixToFile
) where


import System.IO
import Matrix


readMatrixFromFile :: FilePath -> IO Matrix
readMatrixFromFile filePath = do
    content <- readFile filePath
    let rows = lines content
        matrix = map (map read . words) rows :: [[Double]]
    return (Matrix (length matrix) (length (head matrix)) matrix)


writeMatrixToFile :: FilePath -> Matrix -> IO ()
writeMatrixToFile filePath (Matrix _ _ matrix) = do
    let content = unlines $ map (unwords . map show) matrix
    writeFile filePath content