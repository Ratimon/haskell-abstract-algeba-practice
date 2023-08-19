module Main where

data RationalNumber = RationalNumber Integer Integer

-- Function to create a rational number
createRational :: Integer -> Integer -> RationalNumber
createRational p q
    | q /= 0    = RationalNumber p q
    | otherwise = error "Denominator cannot be zero"

-- List comprehension to generate rational numbers
rationalNumbers :: [RationalNumber]
rationalNumbers = [ createRational p q | p <- [-1000..1000], q <- [-1000..1000], q /= 0 ]

-- Function to convert a RationalNumber to a String
rationalToString :: RationalNumber -> String
rationalToString (RationalNumber p q) = show p ++ " / " ++ show q

-- Main function with limited output
main :: IO ()
main = do
    putStrLn "Generated Rational Numbers:"
    let limitedResults = take 100 rationalNumbers
    mapM_ (putStrLn . rationalToString) limitedResults

