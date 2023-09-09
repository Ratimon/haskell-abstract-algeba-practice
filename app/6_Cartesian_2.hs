setA :: [Int]
setA = [1, 2, 3, 4]

setB :: [Int]
setB = [3, 6, 9, 12]

cartesianProduct :: [(Int, Int)]
cartesianProduct = [(x, y) | x <- setA, y <- setB]

-- Print an ordered pair (a, b) as a table row
printProduct :: (Int, Int) -> IO ()
printProduct (a, b) = putStrLn $ "   " ++ show a ++ "   |   " ++ show b ++ "   |   (" ++ show a ++ ", " ++ show b ++ ")"

-- Perform arithmetic computations for specific ordered pairs
computeArithmetic :: [(Int, Int)] -> [Int]
computeArithmetic pairs = [x * y | (x, y) <- pairs]

main :: IO ()
main = do
    putStrLn "Cartesian Product A × B:"
    putStrLn "-------------------------"
    putStrLn "   A   |   B   |  A × B "
    putStrLn "-------------------------"
    mapM_ printProduct cartesianProduct
    
    putStrLn "Arithmetic computations for specific pairs:"
    let specificPairs = [(1, 3), (2, 6), (3, 9), (4, 12)]
    let results = computeArithmetic specificPairs
    print results
