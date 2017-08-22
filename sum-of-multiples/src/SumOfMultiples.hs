module SumOfMultiples (sumOfMultiples) where
import Data.List (nub)
sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit 
  | factors == [] = 0
  | otherwise = answer 
  where answer = sum natural
        natural = nub $ flatten $ map getNatural factors
        flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []
        getNatural num = [num , (num * 2) .. (limit - 1)]

