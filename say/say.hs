module Say (inEnglish) where

inEnglish :: Integral a => a -> Maybe String
inEnglish n
  | n == 0 = Just "zero"
  | n < 0 || (truncate (1e12::Double)) <= n = Nothing
  | otherwise =  Just $ unwords 
    $ map (\(num, mag) -> digits num ++ mag) magnitudes
  where
    magnitudes = reverse 
      $ filter ((/= 0) . fst) 
      $ zip (groups n) ["", " thousand", " million", " billion"]

groups :: Integral a => a -> [a]
groups 0 = []
groups g = re: groups quo
  where
    (quo, re) = divMod g 1000

digits :: Integral a => a -> String
digits n
  | 10 < n && n < 20 = teens n
  | otherwise = joinDigits (h,t,o)
  where
    (h, teen) = divMod n 100
    (t, o) = divMod teen 10

joinDigits :: Integral a => (a, a, a) -> String
joinDigits (0, 0, n) = ones n
joinDigits (0, t, 0) = tens t
joinDigits (h, 0, 0) = ones h ++ " hundred"
joinDigits (0, t, n) = tens t ++ "-" ++ ones n
joinDigits (h, t, 0) = ones h ++ " hundred " ++ tens t
joinDigits (h, t, n) = ones h ++ " hundred " ++ tens t ++ "-" ++ ones n

ones :: Integral a => a -> String
ones n = case n of 
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> ""

teens :: Integral a => a -> String
teens n = case n of
  11 -> "eleven"
  12 -> "twelve"
  13 -> "thirteen"
  14 -> "fourteen"
  15 -> "fifteen"
  16 -> "sixteen"
  17 -> "seventeen"
  18 -> "eighteen"
  19 -> "nineteen"
  _ -> ""

tens :: Integral a => a -> String
tens n = case n of
  1 -> "ten"
  2 -> "twenty"
  3 -> "thirty"
  4 -> "forty"
  5 -> "fifty"
  6 -> "sixty"
  7 -> "seventy"
  8 -> "eighty"
  9 -> "ninety"
  _ -> ""