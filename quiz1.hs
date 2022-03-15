module Quiz1 where

{- | Renders full polynomial term given coefficents
   |
   | > toPoly [2,-3,0,7]
   | 2x^3 - 3x^2 + 7x^0
   | > toPoly [-5, -3, 7, 0, -1, 3]
   | -5x^5 - 3x^4 + 7x^3 - 1x^1 + 3x^0
-}
newtype Poly = Poly [Int]

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
