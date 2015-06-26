module Gigasecond (fromDay) where
import Data.Time.Clock (UTCTime, addUTCTime)

fromDay::UTCTime -> UTCTime
fromDay = addUTCTime (10^(9::Int))