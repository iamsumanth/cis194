module Spring13.Week12.Battlefield 
  ( Army, Battlefield (..)
  ) where

------------------------------------------------------------
-- Battle field

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

instance Monoid Battlefield where
  mempty = Battlefield 0 0
  mappend (Battlefield a1 d1) (Battlefield a2 d2) = Battlefield (a1 + a2) (d1 + d2)
