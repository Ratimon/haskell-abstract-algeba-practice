-- Define the set A
setA :: [Int]
setA = [0, 1, 2, 3, 4]

-- Define the binary operator for subtraction modulo 5
subMod5 :: Int -> Int -> Int
subMod5 a b = (a - b) `mod` 5

-- Generate the cartesian product of the set with itself
cartesianProduct :: [(Int, Int)]
cartesianProduct = [(x, y) | x <- setA, y <- setA]

-- Print an ordered pair (a, b) as a table row
printProduct :: (Int, Int) -> IO ()
printProduct (a, b) = putStrLn $ "   " ++ show a ++ "   |   " ++ show b ++ "   |   (" ++ show a ++ ", " ++ show b ++ ")"

-- -- Map the ordered pairs to A using the binary operator
-- mapToA :: [(Int, Int)] -> [Int]
-- mapToA pairs = [subMod5 a b | (a, b) <- pairs]

-- Map the ordered pairs to A using the binary operator
mapToA :: [(Int, Int)] -> [((Int, Int), Int)]
mapToA pairs = [((a, b) ,subMod5 a b) | (a, b) <- pairs]

-- -- Print a mapped value
-- printMappedValue :: Int -> IO ()
-- printMappedValue a = putStrLn $ show a

-- -- Compute the binary operator for each pair in the cartesian product
-- computeMultMod3 :: [(Int, Int)] -> [((Int, Int), Int)]
-- computeMultMod3 pairs = [((a, b), multMod3 a b) | (a, b) <- pairs]

-- Print a single pair and its result
printResult :: ((Int, Int), Int) -> IO ()
printResult ((a, b), result) = putStrLn $ "(" ++ show a ++ ", " ++ show b ++ ")   |   " ++ show result

-- Display the result
main :: IO ()
main = do
    putStrLn "Ordered Pairs A × A:"
    putStrLn "---------------------"
    mapM_ printProduct cartesianProduct
    
    putStrLn "---------------------"
    putStrLn "Mapped to A using subtraction modulo 5:"
    putStrLn "---------------------"
    mapM_ printResult (mapToA cartesianProduct)


