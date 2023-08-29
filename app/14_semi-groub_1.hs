import Data.Semigroup

data ConcatString x = ConcatString String

instance Semigroup (ConcatString x) where
    (<>) (ConcatString x1) (ConcatString x2) = ConcatString newX
      where
        newX = x1 ++ x2

-- Associativity property: (A op B) op C = A op (B op C)
isAssociative ::  String-> String -> String -> Bool
isAssociative a b c = (a <> b) <> c == a <> (b <> c)

main :: IO ()
main = do
     putStrLn $ "Is concatenation of 'foo', 'bar', and 'baz' associative? " ++ show (isAssociative "foo" "bar" "baz")