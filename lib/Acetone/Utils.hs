{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
{-| General utilities, useful when programming graphics. |-}
module Acetone.Utils where

import GHC.Clock (getMonotonicTime)
import Control.Concurrent (threadDelay)

-- | Shift and scale a bounded value to a different set of bounds.
align :: Fractional q => q -> q -> q -> q -> q -> q
align xmin xmax ymin ymax x = y
  where y = (x - xmin) / (xmax - xmin) * (ymax - ymin) + ymin

-- | Restrict a value between two bounds
clamp :: Fractional q => Ord q => q -> q -> q -> q
clamp xmin xmax x
  | x > xmax = xmax
  | x < xmin = xmin
  | otherwise = x

-- | Monotonic time in ms from unspecified starting point.
-- Hence only relevant to relative time.
monotonicMillis :: IO Double
monotonicMillis = (1000.0 *) <$> getMonotonicTime

-- | A precise `threadDelay` function that observes and accounts for inaccuracies.
preciseDelay :: Double -> IO ()
--preciseDelay millis = intermitentSleep millis 5 5 0 1 >>= spinLock
-- NOTE: intermittentSleep says it sleeps for less time than it actually does.
--       i.e. the remaining sleep time is too long.  We don't account for the time
--       of the arithmetic and function calls, only the time by threadDelay,
--       but this should take so little time its not important.  Figure this out
--       in the future.  see: https://blat-blatnik.github.io/computerBear/making-accurate-sleep-function/
preciseDelay millis = do
  tick <- monotonicMillis
  _ <- intermitentSleep millis 5 5 0 1
  tock <- monotonicMillis
  let remaining = millis - (tock - tick)
  if remaining > 0
    then spinLock remaining
    else pure ()

-- | Sleeps for roughly 1ms up until it is no longer safe to rely on sleeping.
-- Returns remaining time not slept in ms, to be passed on to a spin-lock sleeper.
intermitentSleep :: Double -> Double -> Double -> Double -> Int -> IO Double
intermitentSleep remaining estimate mean m2 count
  | estimate > remaining = pure remaining
  | otherwise = do
    observed <- subtract <$> monotonicMillis <*> (threadDelay 1000 >> monotonicMillis)
    let delta = observed - mean
    let μ = mean + delta / (fromIntegral . succ) count
    let m2' = m2 + delta * (observed - μ)
    let σ = sqrt $ m2 / fromIntegral count
    intermitentSleep (remaining - observed) (μ + σ) μ m2' (succ count)

-- | Idle for the remaining time (ms) with a spin-lock.
spinLock :: Double -> IO ()
spinLock remaining = monotonicMillis >>= spinLock' where
  spinLock' :: Double -> IO ()
  spinLock' start = (subtract start <$> monotonicMillis)
                >>= \delta -> if delta < remaining
                                then spinLock' start
                                else pure ()