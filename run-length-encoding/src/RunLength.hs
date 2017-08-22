module RunLength (decode, encode) where

import Data.List
import Data.Char

decode :: String -> String
decode "" = ""
decode text = de $ span isDigit text
  where
    de ("", x:xs)  = x : decode xs
    de (num, x:xs) = replicate (read num) x ++ decode xs

encode :: String -> String
encode = concatMap en . group
  where
    en [x] = [x]
    en s@(x:xs) = (show $ length s) ++ [x]
