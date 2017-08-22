module DNA (toRNA) where

import Control.Monad

toRNA :: String -> Maybe String
toRNA = foldM translate ""
  where
    translate acc 'G' = Just (acc ++ "C")
    translate acc 'C' = Just (acc ++ "G")
    translate acc 'T' = Just (acc ++ "A")
    translate acc 'A' = Just (acc ++ "U")
    translate _ _ = Nothing
