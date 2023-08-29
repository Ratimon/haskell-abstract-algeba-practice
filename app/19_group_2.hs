import Data.Semigroup
import Data.Monoid

-- Define a type for polynomials
newtype Polynomial = Polynomial [Int] deriving (Eq, Show)

-- Define a custom Semigroup instance for Polynomial (addition of polynomials)
instance Semigroup Polynomial where
    Polynomial p1 <> Polynomial p2 = Polynomial (addLists p1 p2)
        where
            addLists [] ys = ys
            addLists xs [] = xs
            addLists (x:xs) (y:ys) = (x + y) : addLists xs ys

instance Monoid Polynomial where
    mempty = Polynomial [0,0,0]

-- Function to find the additive inverse of a polynomial
inverse :: Polynomial -> Polynomial
inverse (Polynomial p) = Polynomial (map negate p)

-- Check if polynomial addition is associative
checkAssociativity :: Polynomial -> Polynomial -> Polynomial -> Bool
checkAssociativity p q r = (p <> q) <> r == p <> (q <> r)

-- Check if polynomial addition has an identity element
checkIdentity :: Polynomial -> Bool
checkIdentity p = (p <> mempty == p) && (mempty <> p == p)

-- Check if polynomial addition has an inverse element
checkInverse :: Polynomial -> Bool
checkInverse p = (p <> inverse p == mempty) && (inverse p <> p == mempty)

main :: IO ()
main = do
    let poly1 = Polynomial [1, 2, 3]  -- x^2 + 2x + 3
    let poly2 = Polynomial [3, 4, 5]  -- 3x^2 4x + 5

    putStrLn "Properties of Polynomials under Addition"
    putStrLn "----------------------------------------"

    putStrLn "1. Closure:"
    putStrLn $ "Polynomial + Polynomial: " ++ show (poly1 <> poly2)

    putStrLn "\n2. Associativity:"
    putStrLn $ "Associative for (poly1 + poly2) + poly2: " ++ show (checkAssociativity (poly1 <> poly2) poly2 poly2)
    putStrLn $ "Associative for poly1 + (poly2 + poly2): " ++ show (checkAssociativity poly1 (poly2 <> poly2) poly2)

    putStrLn "\n3. Identity Element:"
    putStrLn $ "poly1 <> (inverse poly1) => " ++ show (poly1 <> (inverse poly1))
    putStrLn $ "Identity for poly1: " ++ show (checkIdentity poly1)
    putStrLn $ "Identity for poly2: " ++ show (checkIdentity poly2)

    putStrLn "\n4. Inverse Element:"
    putStrLn $ "Inverse: " ++ show (inverse poly1)

    putStrLn "\n5. Inverse Operation:"
    putStrLn $ "Inverse for poly1: " ++ show (checkInverse poly1)
    putStrLn $ "Inverse for poly2: " ++ show (checkInverse poly2)

