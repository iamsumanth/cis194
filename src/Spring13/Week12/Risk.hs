{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spring13.Week12.Risk 
  ( DieValue
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

------------------------------------------------------------
-- Risk

-- threeInts :: Rand StdGen DieValue
-- threeInts = 
--   getRandom >>= (\x -> return [x])


type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield attackers defenders) = setBattle (getDieForAttacker attackers) (getDieForDefender defenders)


setBattle :: Rand StdGen [DieValue] -> Rand StdGen [DieValue] -> Rand StdGen Battlefield
setBattle attackerDies defenderDies = getAvailableBattlefield (sortDies attackerDies) (sortDies defenderDies)


getAvailableBattlefield :: Rand StdGen [DieValue] -> Rand StdGen [DieValue] -> Rand StdGen Battlefield
getAvailableBattlefield attackerPower defenderPower = 
  (comparePowers attackerPower defenderPower) >>= (\(attackerWon, defenderWon) -> 
    return (Battlefield {attackers = attackerWon, defenders = defenderWon} ))


comparePowers :: Rand StdGen [DieValue] -> Rand StdGen [DieValue] -> Rand StdGen (Int, Int)
comparePowers attackerPower defenderPower = compareDies 
  (attackerPower >>= (\attackerResult ->
  defenderPower >>= (\defenderResult -> return (attackerResult, defenderResult))))


compareDies :: Rand StdGen ([DieValue], [DieValue]) -> Rand StdGen (Int, Int)
compareDies dies = 
  dies >>= (\(attacker, defender) -> return ((compareEveryDieValue attacker defender (>)), (compareEveryDieValue attacker defender (<=))))


compareEveryDieValue :: [DieValue] -> [DieValue] -> (DieValue -> DieValue -> Bool) -> Int
compareEveryDieValue attacker defender predicate = sum . concat $ zipWith (\x y -> [1 | x `predicate` y]) attacker defender

sortDies :: Rand StdGen [DieValue] -> Rand StdGen [DieValue]
sortDies dies =  dies >>= (\dieValues -> return (reverse (sort dieValues)))

getDieForAttacker :: Army -> Rand StdGen [DieValue]
getDieForAttacker 1 = getOneDie
getDieForAttacker 2 = getTwoDies
getDieForAttacker _ = getThreeDies


getDieForDefender :: Army -> Rand StdGen [DieValue]
getDieForDefender 1 = getOneDie
getDieForDefender _ = getTwoDies


getOneDie :: Rand StdGen [DieValue]
getOneDie = die >>= (\die1 -> return [die1])

getTwoDies :: Rand StdGen [DieValue]
getTwoDies = die >>= (\die1 -> die >>= (\die2 -> return [die1, die2]))

getThreeDies :: Rand StdGen [DieValue]
getThreeDies = die >>= (\die1 -> die >>= (\die2 -> die >>= (\die3 -> return [die1, die2, die3])))



getAttackers :: Battlefield -> Army
getAttackers battlefield = attackers battlefield

getDefenders :: Battlefield -> Army
getDefenders battlefield = defenders battlefield

allowedAttackers :: Army -> Army
allowedAttackers availableAttackers
    | (availableAttackers >= 3) = 3
    | otherwise = availableAttackers


allowedDefenders :: Army -> Army
allowedDefenders availabeDefenders
    | (availabeDefenders >= 2) = 2
    | otherwise = availabeDefenders
