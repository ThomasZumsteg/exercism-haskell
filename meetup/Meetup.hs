module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (fromGregorian, gregorianMonthLength, Day)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Schedule = Teenth | First | Second | Third | Fourth | Last

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Sunday | Saturday deriving (Show, Eq)

toWeekDay :: Day -> Weekday
toWeekDay d = 
  case last3 $ toWeekDate d of
    1 -> Monday
    2 -> Tuesday 
    3 -> Wednesday 
    4 -> Thursday 
    5 -> Friday 
    6 -> Saturday
    7 -> Sunday
  where last3 (_, _, c) = c

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay meet day year month = case meet of
  Teenth -> inRange [13..19]
  First -> inRange [1..7]
  Second -> inRange [8..14]
  Third -> inRange [15..21]
  Fourth -> inRange [22..29]
  Last -> inRange [daysInMonth, daysInMonth-1..daysInMonth-7]
  where
    daysInMonth = gregorianMonthLength year month
    inRange = head . filter ((== day) . toWeekDay) . dates
      where
        dates = map (fromGregorian year month)