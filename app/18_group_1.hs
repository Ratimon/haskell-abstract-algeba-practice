import Data.List (isInfixOf)

-- Check if string 'a' is the inverse of string 'b'
isInverse :: String -> String -> Bool
isInverse a b = a ++ b == ""   -- Concatenating an empty string is the identity

-- Check if a set of strings forms a group under concatenation
isGroup :: [String] -> Bool
isGroup strings = all (\a -> any (\b -> isInverse a b) strings) strings

-- Example strings
exampleStrings :: [String]
exampleStrings = ["foo", "bar", "baz"]

main :: IO ()
main = do
  putStrLn "Strings under Concatenation:"
  putStrLn "---------------------------"

  putStrLn "Example Strings:"
  print exampleStrings

  putStrLn "Is a Group?"
  print $ isGroup exampleStrings
