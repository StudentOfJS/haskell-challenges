module Acronym (abbreviate) where
import Data.List.Split (splitOn)
import Data.List
import Data.Char (toUpper)
upperFirst:: String -> String
upperFirst str =  let upperFirstForEachWord (x:xs) = toUpper x : xs
                  in unwords  (map upperFirstForEachWord (words str))
fixAllCaps :: [Char] -> [Char]
fixAllCaps xs = case xs of  [] -> []
                            all (`elem` ['A'..'Z']) xs -> x
                            xs -> xs

abbreviate :: [Char] -> [Char]
abbreviate str =
  let splitDelim = splitOn "-"
      getUpper xs =  map (checkallCaps ) $ map upperFirst $ splitDelim xs
      checkallCaps = all (xs `elem` ['A'..'Z'])
      getAccronym xs = [x |x<-xs, x `elem` ['A'..'Z']]
      getAll xs = getAccronym $ getUpper xs
  in getAll (upperFirst str) 
