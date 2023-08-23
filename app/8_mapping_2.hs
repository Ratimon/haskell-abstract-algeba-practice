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

-- Filter the cartesian product using the mapping
filteredCartesianProduct :: [(Int, String)]
filteredCartesianProduct = filter (\(n, shape) -> n == mapToSides shape) cartesianProduct

-- Print a single mapping line
printMapping :: (Int, String) -> IO ()
printMapping (n, shape) = putStrLn ("|     " ++ show n ++ "      |   " ++ shape ++ "   |")

-- Display the filtered mapping
main :: IO ()
main = do
    putStrLn "Filtered Mapping:"
    putStrLn "-----------------"
    putStrLn "|  Integer  |  Shape    |"
    putStrLn "-----------------"
    mapM_ printMapping filteredCartesianProduct
    putStrLn "-----------------"

    print cartesianProduct
    print filteredCartesianProduct