-- Define sets
setA :: [Int]
setA = [1, 2, 3, 4]

setB :: [Int]
setB = [3, 6, 9, 12]

-- Calculate Cartesian product A × B
cartesianProduct :: [(Int, Int)]
cartesianProduct = [(x, y) | x <- setA, y <- setB]

-- Perform arithmetic computations for specific ordered pairs
computeArithmetic :: [(Int, Int)] -> [Int]
computeArithmetic pairs = [x * y | (x, y) <- pairs]

-- Display results
main :: IO ()
main = do
    putStrLn "Cartesian Product A × B:"
    print cartesianProduct
    putStrLn "Arithmetic computations for specific pairs:"
    let specificPairs = [(1, 3), (2, 6), (3, 9), (4, 12)]
    let results = computeArithmetic specificPairs
    print results
