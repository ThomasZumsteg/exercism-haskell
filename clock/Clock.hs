module Clock (fromHourMin, toString) where
import Text.Printf

data Clock = Clock Int Int deriving (Eq, Show)

instance Num Clock where
  (Clock h m) + (Clock h' m') = fromHourMin (h + h') (m + m')
  (Clock h m) - (Clock h' m') = fromHourMin (h - h') (m - m')
  fromInteger = fromHourMin 0 . fromInteger
  _ * _ = undefined
  abs _ = undefined
  signum _ = undefined

fromHourMin :: Int -> Int -> Clock
fromHourMin h m =
  let
    (dh, m') = divMod m 60
    h' = mod (h + dh) 24
  in Clock h' m'

toString :: Clock -> String
toString (Clock h m) = printf "%02d:%02d" h m
