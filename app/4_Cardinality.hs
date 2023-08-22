-- Check if two finite sets have different cardinalities
differentCardinalities :: Eq a => [a] -> [a] -> Bool
differentCardinalities setA setB = length setA /= length setB

-- Check if two finite sets are equal
areSetsEqual :: Eq a => [a] -> [a] -> Bool
areSetsEqual setA setB = not (differentCardinalities setA setB) && all (`elem` setB) setA

main :: IO ()
main = do
    let setA = [5, 9, 10]
    let setB = [1, 2, 3, 4, 5]
    let setC = [1, 2, 3, 4, 5, 6]
    let setD = [1, 2, 3, 4, 5, 6]

    putStrLn $ "Set A: " ++ show setA
    putStrLn $ "Set B: " ++ show setB
    putStrLn $ "Set C: " ++ show setC

    putStrLn $ "Are sets A and B equal? " ++ show (areSetsEqual setA setB)
    putStrLn $ "Are sets A and C equal? " ++ show (areSetsEqual setA setC)
    putStrLn $ "Are sets C and D equal? " ++ show (areSetsEqual setC setD)