module OCR (convert) where

import Data.List (elemIndex, transpose, intercalate)
import Data.List.Split (chunksOf)

makeDigit :: [String] -> String
makeDigit digit = case elemIndex digit ocr_digits of
      Just n -> show n
      Nothing -> "?"

converLine :: [String] -> String
converLine = concat . map makeDigit . transpose . map (chunksOf 3)

convert :: String -> String
convert = intercalate "," . map converLine . chunksOf 4 . lines

ocr_digits :: [[String]]
ocr_digits = [
    [ " _ "
    , "| |"
    , "|_|"
    , "   " ],
    [ "   "
    , "  |"
    , "  |"
    , "   "],
    [ " _ "
    , " _|"
    , "|_ "
    , "   "],
    [ " _ "
    , " _|"
    , " _|"
    , "   " ],
    [ "   "
    , "|_|"
    , "  |"
    , "   " ],
    [ " _ "
    , "|_ "
    , " _|"
    , "   " ],
    [ " _ "
    , "|_ "
    , "|_|"
    , "   "],
    [ " _ "
    , "  |"
    , "  |"
    , "   " ],
    [ " _ "
    , "|_|"
    , "|_|"
    , "   " ],
    [ " _ "
    , "|_|"
    , " _|"
    , "   "]
  ]
