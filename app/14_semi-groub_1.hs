-- Define the strings
stringFoo :: String
stringFoo = "foo"

stringBar :: String
stringBar = "bar"

stringBaz :: String
stringBaz = "baz"

-- Associativity check
isAssociative :: Bool
isAssociative = (stringFoo ++ stringBar) ++ stringBaz == stringFoo ++ (stringBar ++ stringBaz)

-- Main function
main :: IO ()
main = do
    putStrLn $ "Is concatenation of 'foo', 'bar', and 'baz' associative? " ++ show isAssociative
