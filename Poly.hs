module Poly where

newtype Poly = Poly [Int]

-- | Embed a list of integers into a mapped type
--   | to treat them as coefficients on polynomial terms
toPoly :: [Int] -> Poly
toPoly = Poly

instance Show Poly where
  show (Poly xs) =
    leadingPolarity $ snd (foldr renderTerm (0, "") xs)
    where
      leadingPolarity :: String -> String
      leadingPolarity s
        | " + " == polarity = drop3
        | " - " == polarity = "-" ++ drop3
        | otherwise = s
        where
          polarity = take 3 s
          drop3 = drop 3 s

      renderTerm :: Int -> (Int, String) -> (Int, String)
      renderTerm coeff (power, result) =
        ( 1 + power,
          finishedTerm coeff rawTerm ++ result
        )
        where
          rawTerm = (" " ++ show coeff ++ "x^") ++ show power

          finishedTerm :: Int -> String -> String
          finishedTerm coeff term
            | coeff > 0 = " +" ++ term
            | coeff == 0 = ""
            | otherwise = " - " ++ drop 2 term

-- | Add 2 polynomials and re
--   |
--   | addPoly (toPoly [1, 2, 3]) (toPoly [10, 20, 30])
--   | > 11x^2 + 22x^1 + 33x^0
addPoly :: Poly -> Poly -> Poly
addPoly (Poly p1) (Poly p2) =
  Poly $ zipWith (+) p1 p2

-- | Evaluate the value of the polynomial at point x.
-- |
-- | evalPoly (toPoly [4, 3, 5, 7]) 0
-- | > 7
-- | evalPoly (toPoly [4, 3, 5, 7]) 1
-- | > 19
-- | evalPoly (toPoly [4, 3, 5, 7]) 2
-- | > 61
-- | evalPoly (toPoly [4, 3, 5, 7]) 3
-- | > 157
-- | evalPoly (toPoly [4, -3, -5, 7]) 3
-- | > 73
-- | evalPoly (toPoly [4, -3, 0, 7]) (-1)
-- | > 0
evalPoly :: Poly -> Int -> Int
evalPoly (Poly xs) x =
  fst $ foldr evalTerm (0, 0) xs
  where
    evalTerm :: Int -> (Int, Int) -> (Int, Int)
    evalTerm coeff (value, power) =
      (value + coeff * x ^ power, power + 1)
