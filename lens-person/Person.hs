{-# LANGUAGE TemplateHaskell #-}

module Person where

import           Data.Time.Calendar
import           Control.Lens

data Person = Person {
      _name    :: Name,
      _born    :: Born,
      _address :: Address
    }

data Name = Name {
      _foreNames :: String, -- Space separated
      _surName   :: String
    }

data Born = Born {
      _bornAt :: Address,
      _bornOn :: Day
    }

data Address = Address {
      _street      :: String,
      _houseNumber :: Int,
      _place       :: String, -- Village / city
      _country     :: String
    }

-- Valid values of Gregorian are those for which 'Data.Time.Calendar.fromGregorianValid'
-- returns Just.
data Gregorian = Gregorian {
      _year  :: Integer,
      _month :: Int,
      _day   :: Int
    }

makeLenses ''Person
makeLenses ''Name
makeLenses ''Born
makeLenses ''Address
makeLenses ''Gregorian

bornStreet :: Born -> String
bornStreet = view (bornAt . street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = set (address . street) 

setBirthMonth :: Int -> Person -> Person
setBirthMonth = set (born . bornOn . from gregorianLens . month)

-- | Transform both birth and current street names.
renameStreets :: (String -> String) -> Person -> Person
renameStreets s = (birth %~ s) . (current %~ s)
  where
    birth = born . bornAt . street
    current = address . street

gregorianLens :: Iso' Gregorian Day
gregorianLens = iso gregorianToDay dayToGregorian
  where
    gregorianToDay (Gregorian y m d) = fromGregorian y m d
    dayToGregorian day' = let (y, m, d) = toGregorian day' in Gregorian y m d

