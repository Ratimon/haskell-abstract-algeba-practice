import Data.Complex

-- Check if a given number is transcendental
isTranscendental :: RealFloat a => Complex a -> Bool
isTranscendental z = not (isAlgebraic z)

--  Checks if there's at least one algebraic polynomial in the algebraicPolynomials list for which the given complex number z is a root
--  (i.e., the polynomial evaluates to 0 at z). If such a polynomial exists, then z is considered algebraic; otherwise, it's not.
isAlgebraic :: RealFloat a => Complex a -> Bool
isAlgebraic z = any (\p -> evaluatePolynomial p z == 0) algebraicPolynomials

-- Sample algebraic polynomials (replace with actual polynomials)
algebraicPolynomials :: RealFloat a => [Complex a -> a]
algebraicPolynomials = [poly1, poly2, poly3]  -- Define your algebraic polynomials here

-- Evaluate a polynomial at a given complex number
evaluatePolynomial :: RealFloat a => (Complex a -> a) -> Complex a -> a
evaluatePolynomial poly z = poly z

-- Sample algebraic polynomials
poly1 :: RealFloat a => Complex a -> a
poly1 z = realPart z ^ 2 - 2 * imagPart z

poly2 :: RealFloat a => Complex a -> a
poly2 z = realPart z ^ 3 + imagPart z

poly3 :: RealFloat a => Complex a -> a
poly3 z = realPart z - 5 * imagPart z ^ 2

-- Sample usage
main :: IO ()
main = do
    let complexNumber = 2 :+ 3  -- Create a complex number 2 + 3i

    -- Check if the complex number is transcendental
    let isTranscendentalNumber = isTranscendental complexNumber
    putStrLn $ "Is the complex number transcendental? " ++ show isTranscendentalNumber

    -- Evaluate the sample algebraic polynomials at the complex number
    --  (i.e., (2^2) - 2*3 = 4 - 6 = -2, which is not equal to 0)
    let evalPoly1 = evaluatePolynomial poly1 complexNumber
    putStrLn $ "Result of evaluating poly1 at the complex number: " ++ show evalPoly1

    --  (i.e., (2^3) + 3 = 8 + 3 = 11, which is not equal to 0)
    let evalPoly2 = evaluatePolynomial poly2 complexNumber
    putStrLn $ "Result of evaluating poly2 at the complex number: " ++ show evalPoly2

    --  (i.e., 2 - 5*(3^2) = 2 - 45 = -43, which is not equal to 0)
    let evalPoly3 = evaluatePolynomial poly3 complexNumber
    putStrLn $ "Result of evaluating poly3 at the complex number: " ++ show evalPoly3