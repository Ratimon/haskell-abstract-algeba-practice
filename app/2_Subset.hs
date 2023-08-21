import Data.Complex
import qualified Data.Set as S

-- Define the data types for different number systems
-- data RealNumber = RealNumber Double
-- data RationalNumber = RationalNumber Rational 
-- data ComplexNumber = ComplexNumber (Complex Double)
-- -- Define the Show instance for ComplexNumber
-- instance Show ComplexNumber where
--     show (ComplexNumber (x :+ y)) = show x ++ " + " ++ show y ++ "i"

-- Check subset relationships
isSubset :: Ord a => [a] -> [a] -> Bool
isSubset xs ys = all (`S.member` yset) xs where
  yset = S.fromList ys


main :: IO ()
main = do
    let integers = [1..3]
        rationalNumbers = [-3/2, -1, 0, 1/2, 2/3, 1 ,2 , 3]
        realNumbers = [-3/2, -1, 0, 1/2, 2/3, 1 ,2 , 3, -2.5, 0, 3.14]
        complex = [1 :+ 2, 2 :+ 3]
        -- complexNumbers = [ComplexNumber (2 :+ 3), ComplexNumber (0 :+ 1)]

    putStrLn "Integers are a subset of Rational Numbers:"
    print (isSubset integers rationalNumbers)
    
    putStrLn "Rational Numbers are a subset of Real Numbers:"
    print (isSubset rationalNumbers realNumbers)
    