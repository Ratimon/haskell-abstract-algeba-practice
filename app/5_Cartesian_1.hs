setA :: [Int]
setA = [1, 2, 3]

setB :: [Char]
setB = ['x', 'y', 'z']

cartesianProduct :: [(Char, Int)]
cartesianProduct = [(x, y) | x <- setB, y <- setA]

-- Print an ordered pair (a, b) as a table row
printProduct :: (Char, Int) -> IO ()
printProduct (a, b) = putStrLn $ "   " ++ show a ++ "   |   " ++ show b ++ "   |   (" ++ show a ++ ", " ++ show b ++ ")"

main :: IO ()
main = do
    putStrLn "Cartesian Product B × A:"
    putStrLn "-------------------------"
    putStrLn "   B   |   A   |  B × A "
    putStrLn "-------------------------"
    mapM_ printProduct cartesianProduct
    putStrLn "-------------------------"

