module Bob (responseFor) where

responseFor :: String -> String
responseFor str
  | null noSpace = "Fine. Be that way!"
  | testSomeLow && testQuestion || not testContainsChar && testQuestion = "Sure."
  | not testSomeLow && testContainsChar = "Whoa, chill out!"
  | otherwise = "Whatever."
  where noSpace = [x | x <- str, x `elem` (['A'..'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "?")]
        testSomeLow = any (`elem` ['a' .. 'z']) noSpace
        testContainsChar = any (`elem` (['a' .. 'z'] ++ ['A' .. 'Z'])) noSpace
        testQuestion = last noSpace == '?'