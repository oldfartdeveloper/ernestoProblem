module Quiz1 where
newtype Poly = Poly [Int]

{- | Embed a list of integers into a mapped type
   | to treat them as coefficients on polynomial terms
-}
toPoly :: [Int] -> Poly
toPoly = Poly

instance Show Poly where

  {- | Renders full polynomial term given coefficents
    |
    | > toPoly [2,-3,0,7]
    | 2x^3 - 3x^2 + 7x^0
    | > toPoly [-5, -3, 7, 0, -1, 3]
    | -5x^5 - 3x^4 + 7x^3 - 1x^1 + 3x^0
  -}
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

{- | Add 2 polynomials
   |
   | addPoly [1, 2, 3] [10, 20, 30]
   | > [11, 22, 33]
-}
addPoly :: [Int] -> [Int] -> [Int]
addPoly = zipWith (+)

{- | Evaluate the value of the polynomial at point x
   |
   | evalPoly [4, 3, 5, 7] 0
   | > 7
   | evalPoly [4, 3, 5, 7] 1
   | > 19
   | evalPoly [4, 3, 5, 7] 2
   | > 61
   | evalPoly [4, 3, 5, 7] 3
   | > 157
   | evalPoly [4, -3, -5, 7] 3
   | > 73
   | evalPoly [4, -3, 0, 7] (-1)
   | > 5
-}
evalPoly :: [Int] -> Int -> Int
evalPoly xs x =
  fst $ foldr evalTerm (0, 0) xs
  where
    evalTerm :: Int -> (Int, Int) -> (Int, Int)
    evalTerm coeff (value, power) =
      (value + coeff * x ^ power, power + 1)
