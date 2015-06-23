module DNA (toRNA) where

toRNA::String -> String
toRNA = map translate
  where translate c = case c of
          'A' -> 'U'
          'T' -> 'A'
          'G' -> 'C'
          'C' -> 'G'
          _ -> error ( "invalid nucleotide " )