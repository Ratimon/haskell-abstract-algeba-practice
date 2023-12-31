setA :: [Int]
setA = [0, 1, 2]

-- Define the binary operator for multiplication modulo 3
multMod3 :: Int -> Int -> Int
multMod3 a b = (a * b) `mod` 3

cartesianProduct :: [(Int, Int)]
cartesianProduct = [(x, y) | x <- setA, y <- setA]

-- Print an ordered pair (a, b) as a table row
printProduct :: (Int, Int) -> IO ()
printProduct (a, b) = putStrLn $ "   " ++ show a ++ "   |   " ++ show b ++ "   |   (" ++ show a ++ ", " ++ show b ++ ")"

-- Pick a subset of ordered pairs that defines a * b mod 3
pickSubset :: [(Int, Int)] -> [(Int, Int)]
pickSubset ls = filter (\(a, b) -> multMod3 a b `elem` setA) ls

-- Print subset as a table row
printSubset :: (Int, Int) -> IO ()
printSubset (a, b) = putStrLn $ "(" ++ show a ++ ", " ++ show b ++ ")"

-- Compute the binary operator for each pair in the cartesian product
computeMultMod3 :: [(Int, Int)] -> [((Int, Int), Int)]
computeMultMod3 pairs = [((a, b), multMod3 a b) | (a, b) <- pairs]

-- Print a single pair and its result
printResult :: ((Int, Int), Int) -> IO ()
printResult ((a, b), result) = putStrLn $ "(" ++ show a ++ ", " ++ show b ++ ")   |   " ++ show result

main :: IO ()
main = do
    putStrLn "Cartesian Product A × A:"
    putStrLn "-------------------------"
    putStrLn "   A   |   A   |  A × A "
    putStrLn "-------------------------"
    mapM_ printProduct cartesianProduct

    putStrLn "------------------------------"
    putStrLn "Pick a subset of ordered pairs that defines a * b mod 3."
    putStrLn "------------------------------"
    mapM_ printSubset (pickSubset cartesianProduct)

    putStrLn "------------------------------"
    putStrLn "Multiplication Modulo 3 Table:"
    putStrLn "------------------------------"
    putStrLn "   a   |   b   |  a * b mod 3"    
    putStrLn "------------------------------"
    mapM_ printResult (computeMultMod3 cartesianProduct)
