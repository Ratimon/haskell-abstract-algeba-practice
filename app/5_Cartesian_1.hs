-- Define sets A and B
setA :: [Int]
setA = [1, 2, 3]

setB :: [Char]
setB = ['x', 'y', 'z']

-- Calculate Cartesian product B × A
cartesianProduct :: [(Char, Int)]
cartesianProduct = [(x, y) | x <- setB, y <- setA]

-- Print an ordered pair (b, a) as a table row
printPair :: (Char, Int) -> IO ()
printPair (b, a) = putStrLn $ "   " ++ [b] ++ "   |   " ++ show a ++ "   |   (" ++ [b] ++ ", " ++ show a ++ ")"

-- Display Cartesian product as a table
main :: IO ()
main = do
    putStrLn "Cartesian Product B × A:"
    putStrLn "-------------------------"
    putStrLn "   B   |   A   |  B × A "
    putStrLn "-------------------------"
    mapM_ printPair cartesianProduct
    putStrLn "-------------------------"

