module House ( rhyme ) where

verse :: Int -> String
verse v = concat $ first_line v : [refrain r | r <- reverse [0..(v-1)]]

first_line :: Int -> String
first_line l = case l of
  0  -> "This is the house that Jack built.\n"
  1  -> "This is the malt\n"
  2  -> "This is the rat\n"
  3  -> "This is the cat\n"
  4  -> "This is the dog\n"
  5  -> "This is the cow with the crumpled horn\n"
  6  -> "This is the maiden all forlorn\n"
  7  -> "This is the man all tattered and torn\n"
  8  -> "This is the priest all shaven and shorn\n"
  9  -> "This is the rooster that crowed in the morn\n"
  10 -> "This is the farmer sowing his corn\n"
  11 -> "This is the horse and the hound and the horn\n"
  _  -> error "Out of verses!"

refrain :: Int -> String
refrain r = case r of
  0  -> "that lay in the house that Jack built.\n"
  1  -> "that ate the malt\n"
  2  -> "that killed the rat\n"
  3  -> "that worried the cat\n"
  4  -> "that tossed the dog\n"
  5  -> "that milked the cow with the crumpled horn\n"
  6  -> "that kissed the maiden all forlorn\n"
  7  -> "that married the man all tattered and torn\n"
  8  -> "that woke the priest all shaven and shorn\n"
  9  -> "that kept the rooster that crowed in the morn\n"
  10 -> "that belonged to the farmer sowing his corn\n"
  _  -> error "Out of lines!"


rhyme :: String
rhyme = unlines [verse n | n <- [0..11]] 
