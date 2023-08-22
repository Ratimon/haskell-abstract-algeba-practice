-- Define sets
setA :: [Int]
setA = [1, 2, 3, 4, 5, 6]

setB :: [String]
setB = ["even", "odd"]

-- Generate the cartesian product of sets A and B
cartesianProduct :: [(Int, String)]
cartesianProduct = [(x, y) | x <- setA, y <- setB]

-- Define the mapping function for integers
mapToEvenOdd :: Int -> String
mapToEvenOdd n
    | even n = "even"
    | otherwise = "odd"

-- Filter the cartesian product using mapToEvenOdd
filteredCartesianProduct :: [(Int, String)]
filteredCartesianProduct = filter (\(n, mapping) -> mapping == mapToEvenOdd n) cartesianProduct

printMapping :: (Int, String) -> IO ()
printMapping (n, mapping) = putStrLn ("|     " ++ show n ++ "      |   " ++ mapping ++ "   |")

-- Display the filtered mapping
main :: IO ()
main = do
    putStrLn "Filtered Mapping:"
    putStrLn "------------------"
    putStrLn "|  Integer  |  Mapping  |"
    putStrLn "------------------"
    mapM_ printMapping filteredCartesianProduct
    putStrLn "------------------"

    -- print cartesianProduct
    -- print filteredCartesianProduct







