{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spring13.Week12.RiskTypes 
  ( Army, Battlefield (..), DieValue, die
  ) where

import Control.Monad.Random

------------------------------------------------------------
-- Battle field

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

instance Monoid Battlefield where
  mempty = Battlefield 0 0
  mappend (Battlefield a1 d1) (Battlefield a2 d2) = Battlefield (a1 + a2) (d1 + d2)


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
