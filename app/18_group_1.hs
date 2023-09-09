import Data.Monoid

newtype ConcatString = ConcatString String deriving (Eq, Show)

instance Semigroup ConcatString where
    (ConcatString s1) <> (ConcatString s2) = ConcatString (s1 ++ s2)

-- Define a custom Monoid instance for ConcatString
instance Monoid ConcatString where
    mempty = ConcatString ""
    mappend = (<>)

-- Function to check if an inverse exists for a ConcatString
inverseCheck :: ConcatString -> Bool
inverseCheck (ConcatString str) = ConcatString (str ++ str) == mempty

main :: IO ()
main = do
    putStrLn "Checking Monoid Properties for Strings under Concatenation"
    putStrLn "-------------------------------------------------------"
    
    let str1 = ConcatString "hello"
    let str2 = ConcatString "world"
    let str3 = ConcatString "foo"
    
    putStrLn $ "Concatenating '" ++ show str1 ++ "' and '" ++ show str2 ++ "': " ++ show (str1 <> str2)
    putStrLn $ "Concatenating '" ++ show str1 ++ "' and mempty: " ++ show (str1 <> mempty)
    
    putStrLn $ "Is Group?: Inverse check for '" ++ show str3 ++ "': " ++ show (inverseCheck str3)