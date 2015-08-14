module FoodChain (song) where

song :: String
song = unlines $ map verse [0..7]

verse :: Integer -> String
verse 7 = "I know an old lady who swallowed a horse.\nShe's dead, of course!\n"
verse n = unlines $ first n : map refrain [n, (n-1).. 0] 

first :: Integer -> String
first n = "I know an old lady who swallowed a " ++
  case n of
    0 -> "fly."
    1 -> "spider.\nIt wriggled and jiggled and tickled inside her."
    2 -> "bird.\nHow absurd to swallow a bird!"
    3 -> "cat.\nImagine that, to swallow a cat!"
    4 -> "dog.\nWhat a hog, to swallow a dog!"
    5 -> "goat.\nJust opened her throat and swallowed a goat!"
    6 -> "cow.\nI don't know how she swallowed a cow!"
    _ -> error "Not enough things to eat!"

refrain :: Integer -> String
refrain 0 = "I don't know why she swallowed the fly. Perhaps she'll die."
refrain n = "She swallowed the " ++
  case n of
    1 -> "spider to catch the fly."
    2 -> "bird to catch the spider that wriggled and jiggled and tickled inside her."
    3 -> "cat to catch the bird."
    4 -> "dog to catch the cat."
    5 -> "goat to catch the dog."
    6 -> "cow to catch the goat."
    _ -> error "Not enough things to eat!"