setA :: [Int]
setA = [1, 2, 3]

setB :: [Char]
setB = ['x', 'y', 'z']

-- Define a function from A to B that is not surjective and not injective
functionAB :: Int -> Char
functionAB 1 = 'x'
functionAB 2 = 'x'
functionAB 3 = 'y'

-- Print a single mapping line
printFunction :: Int -> IO ()
printFunction a = putStrLn ("|   " ++ show a ++ "   |   " ++ [functionAB a] ++ "   |")

-- Display the mapping
main :: IO ()
main = do
    putStrLn "Mapping from A to B:"
    putStrLn "---------------------"
    putStrLn "|   A   |   B   |"
    putStrLn "---------------------"
    mapM_ printFunction setA
    putStrLn "---------------------"