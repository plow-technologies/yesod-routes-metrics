module Yesod.Routes.Stats 
  ( percentile
  , percentileWithSort 
  ) where

import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Generic          as VG
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as VUM

-- | xs is assumed to be sorted
percentile :: Double -> VU.Vector Double -> Double
{-# INLINE percentile #-}
percentile p xs
    | VU.length xs == 0 = 0
    | pos > fromIntegral (VU.length xs) = VU.last xs
    | pos' < 1 = VU.head xs
    | otherwise =
        lower + (pos - fromIntegral (floor pos :: Int)) * (upper - lower)
  where
    q = clamp p
    pos = q * (1 + fromIntegral (VU.length xs))
    pos' = truncate pos
    lower = VU.unsafeIndex xs (pos' - 1)
    upper = VU.unsafeIndex xs pos'

-- | sorts xs for you but it needs to be an Unboxed Mutable Vector
percentileWithSort :: Double -> VUM.IOVector Double -> IO Double
percentileWithSort p xs = do 
  VA.sort xs
  xs' <- VG.unsafeFreeze xs :: IO (VU.Vector Double)
  return $ percentile p xs'


clamp :: Double -> Double
{-# INLINE clamp #-}
clamp x | x > 1 = 1
        | x < 0 = 0
        | otherwise = x
