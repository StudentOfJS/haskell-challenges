module Pangram (isPangram) where
import Data.List
import Data.Char

removeNonChar :: String -> String
removeNonChar = filter(`elem` (['a' .. 'z'] ++ ['A' .. 'Z']))

allToLower :: String -> String
allToLower text = [ toLower loweredString | loweredString <- text]

unique :: String -> String
unique = nub . allToLower . removeNonChar

lengthCharString :: String -> Int
lengthCharString = length . unique

isPangram :: String -> Bool
isPangram text = lengthCharString text == 26
