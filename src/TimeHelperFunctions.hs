module TimeHelperFunctions where

import Data.Time

import Data.Time.Clock (UTCTime(..), addUTCTime, secondsToNominalDiffTime)
import Data.Time.Calendar (fromGregorian)

import Types


{------------------------------------------
Contains helper functions to deal with time
-------------------------------------------}

-- Time duration between two absolute times
timeDifferenceAbsolute :: TimeAbsolute -> TimeAbsolute -> TimeRelative
timeDifferenceAbsolute = diffUTCTime

-- Scale a relative time by a factor
scaleTimeRelative :: Double -> TimeRelative -> TimeRelative
scaleTimeRelative x t = realToFrac x * t

-- Add a relative time to an absolute time
addTimeDuration :: TimeAbsolute -> TimeRelative -> TimeAbsolute
addTimeDuration = flip addUTCTime

-- Unit of days
days :: TimeRelative
days = nominalDay

-- Unit of hours
hours :: TimeRelative
hours = 3600

-- Create the UTCTime from 1st September 2024 and add days
utcTimeFrom :: Integer -> UTCTime
utcTimeFrom daysToAdd =
  let utcStartTime = UTCTime (fromGregorian 2024 9 1) 0
  in addUTCTime (daysToNominalDiffTime daysToAdd) utcStartTime

-- Convert days to DiffTime
daysToDiffTime :: Integer -> DiffTime
daysToDiffTime days = secondsToDiffTime (days * 86400)  -- 86400 seconds in a day

-- Convert days to NominalDiffTime
daysToNominalDiffTime :: Integer -> NominalDiffTime
daysToNominalDiffTime days = fromIntegral (days * 86400)  -- 86400 seconds in a day

