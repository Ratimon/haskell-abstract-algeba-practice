import Data.Magma
import Data.Semigroup

-- Define the Magma instance for positive integers using average operation
instance Magma Integer where
    (<<) a b = (a + b) `div` 2

-- Check if the magma operation is associative for positive integers
isAssociativeMagma :: Integer -> Integer -> Integer -> Bool
isAssociativeMagma a b c = (a << b) << c == a << (b << c)

-- Define the Semigroup instance for integers using addition
instance Semigroup Integer where
    (<>) = (+)

-- Check if addition is associative for non-negative integers
isAssociative :: Integer -> Integer -> Integer -> Bool
isAssociative a b c = (a <> b) <> c == a <> (b <> c)

main :: IO ()
main = do
    putStrLn "-------------------------------------------------------"
    putStrLn "-------------------------------------------------------"

    putStrLn "Is the average operation associative for positive integers?"
    let positiveIntegers = [1..]
    putStrLn $ "Associative for 2 ⊕ 3 ⊕ 4: " ++ show (isAssociativeMagma 2 3 4)

    putStrLn "-------------------------------------------------------"

    putStrLn "Is addition associative for for positive integers?"
    putStrLn $ "Associative for 2 ⨁ 3 ⨁ 4: " ++ show (isAssociative 2 3 4)
