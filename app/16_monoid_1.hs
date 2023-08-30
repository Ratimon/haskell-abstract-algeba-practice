import Data.Semigroup
import Data.Monoid
import Numeric.Natural

-- Wrapper type for min(a, b)
newtype MinNatural = MinNatural Natural deriving (Eq, Show)

-- Wrapper type for max(a, b)
newtype MaxNatural = MaxNatural Natural deriving (Eq, Show)

instance Semigroup MinNatural where
    (MinNatural a) <> (MinNatural b) = MinNatural (min a b)

instance Semigroup MaxNatural where
    (MaxNatural a) <> (MaxNatural b) = MaxNatural (max a b)

instance Monoid MinNatural where
    mempty = MinNatural (10^18)  -- Use a large constant value for identity
    mappend = (<>)

instance Monoid MaxNatural where
    mempty = MaxNatural 0
    mappend = (<>)

-- Check associativity for semigroups
checkAssociativity :: (Semigroup a, Show a, Eq a) => a -> a -> a -> IO ()
checkAssociativity a b c = do
    putStrLn $ "Associative for " ++ show a ++ " <> " ++ show b ++ " <> " ++ show c ++ ": " ++ show ((a <> b) <> c == a <> (b <> c))

-- Check identity for monoids
checkIdentity :: (Monoid a, Eq a, Show a) => a -> IO ()
checkIdentity a = do
    putStrLn $ "Identity for " ++ show a ++ ": " ++ show (mappend a mempty == a && mappend mempty a == a)

main :: IO ()
main = do
    putStrLn "Checking Associativity for Semigroups"
    putStrLn "------------------------------------"
    checkAssociativity (MinNatural 3) (MinNatural 4) (MinNatural 5)
    checkAssociativity (MaxNatural 5) (MaxNatural 2) (MaxNatural 7)

    putStrLn "Checking Identity for Monoids"
    putStrLn "-----------------------------"
    checkIdentity (MinNatural 3)
    checkIdentity (MaxNatural 0)

