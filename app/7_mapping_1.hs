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

-- Define a mapping such that the integer maps to the number of sides on the shape
defineMapping :: [(Int, String)] -> [(Int, String)]
defineMapping ls = filter (\(n, mapping) -> mapping == mapToEvenOdd n) ls

printMapping :: (Int, String) -> IO ()
printMapping (n, mapping) = putStrLn ("|     " ++ show n ++ "      |   " ++ mapping ++ "   |")

-- Print a Cartesian Product
printProduct :: (Int, String) -> IO ()
printProduct (n, shape) = putStrLn ("|     " ++ show n ++ "      |   " ++ shape ++ "   |")

-- Print a mapping result
printResult :: (Int, String) -> IO ()
printResult (n, shape) = putStrLn $ "(" ++ show n ++ ", " ++ show shape ++ ") " 

main :: IO ()
main = do

    putStrLn "Cartesian Product A × B:"
    putStrLn "-----------------"
    putStrLn "|  Integer  |  Mapping    |"
    putStrLn "-----------------"
    mapM_ printProduct cartesianProduct
    putStrLn "-----------------"

    putStrLn "------------------------------"
    putStrLn "Mapping"
    putStrLn "------------------------------"
    mapM_ printResult (defineMapping cartesianProduct)






