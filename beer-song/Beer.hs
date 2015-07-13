module Beer (sing, verse) where

verse :: Int -> String
verse 0 = 
  "No more bottles of beer on the wall, no more bottles of beer.\n" ++
  "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
verse 1 = 
  "1 bottle of beer on the wall, 1 bottle of beer.\n" ++
  "Take it down and pass it around, no more bottles of beer on the wall.\n"
verse 2 =
  "2 bottles of beer on the wall, 2 bottles of beer." ++
  "\nTake one down and pass it around, 1 bottle of beer on the wall.\n" 
verse n = 
  show n ++ " bottles of beer on the wall, " ++ 
  show n ++ " bottles of beer.\nTake one down and pass it around, " ++
  show (n - 1) ++ " bottles of beer on the wall.\n"

sing :: Int -> Int -> String
sing start stop = concat $ map (\v -> verse v ++ "\n") (reverse [stop..start])