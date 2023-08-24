-- Define sets
setIntegers :: [Int]
setIntegers = [0, 1, 2, 3, 4, 5, 6, 7, 8]

setPolygons :: [String]
setPolygons = ["triangle", "square", "pentagon", "hexagon", "heptagon", "octagon"]

-- Generate the cartesian product of sets
cartesianProduct :: [(Int, String)]
cartesianProduct = [(x, y) | x <- setIntegers, y <- setPolygons]

-- Define the mapping function for polygons
mapToSides :: String -> Int
mapToSides "triangle" = 3
mapToSides "square" = 4
mapToSides "pentagon" = 5
mapToSides "hexagon" = 6
mapToSides "heptagon" = 7
mapToSides "octagon" = 8

-- Define a mapping such that the integer maps to the number of sides on the shape
defineMapping :: [(Int, String)] -> [(Int, String)]
defineMapping ls = filter (\(n, shape) -> n == mapToSides shape) ls

-- Print a Cartesian Product
printProduct :: (Int, String) -> IO ()
printProduct (n, shape) = putStrLn ("|     " ++ show n ++ "      |   " ++ shape ++ "   |")

-- Print a mapping result
printResult :: (Int, String) -> IO ()
printResult (n, shape) = putStrLn $ "(" ++ show n ++ ", " ++ show shape ++ ") " 

main :: IO ()
main = do
    putStrLn "Cartesian Product:"
    putStrLn "-----------------"
    putStrLn "|  Integer  |  Shape    |"
    putStrLn "-----------------"
    mapM_ printProduct cartesianProduct
    putStrLn "-----------------"

    putStrLn "------------------------------"
    putStrLn "Subsets:"
    putStrLn "------------------------------"
    mapM_ printResult (defineMapping cartesianProduct)