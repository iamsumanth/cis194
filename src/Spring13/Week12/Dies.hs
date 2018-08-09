{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spring13.Week12.Dies
  ( DieValue, die, makeSameSize, sortDies
  ) where

import Control.Monad.Random
import Data.Sort

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

makeSameSize :: [DieValue] -> [DieValue] -> ([DieValue], [DieValue])
makeSameSize arr1 arr2 
    | length arr1 == length arr2 = (arr1, arr2)
    | length arr1 < length arr2 = (arr1, cropList arr2 (length arr1))
    | length arr1 > length arr2 = (cropList arr1 (length arr2), arr2)

sortDies :: Rand StdGen [DieValue] -> Rand StdGen [DieValue]
sortDies dies =  dies >>= (\dieValues -> return (reverse (sort dieValues)))

----------------------------------------------------- 

cropList :: [DieValue] -> Int -> [DieValue]
cropList arr cropTill = take cropTill arr