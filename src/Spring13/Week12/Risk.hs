{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Spring13.Week12.Risk 
  ( DieValue, Battlefield
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


instance Monoid Battlefield where
  mempty = Battlefield 0 0
  mappend (Battlefield a1 d1) (Battlefield a2 d2) = Battlefield (a1 + a2) (d1 + d2)


battle :: Battlefield -> Rand StdGen Battlefield
battle battleField = 
  (\(restingBattlefield, fightingBattlefield) -> 
    (setBattle (getDieForAttacker (attackers fightingBattlefield)) (getDieForDefender (defenders fightingBattlefield))) >>= 
      (\battlefield -> return (mappend battlefield restingBattlefield))
    )
    (getArmyForBattle battleField)
  


getArmyForBattle :: Battlefield -> (Battlefield, Battlefield)
getArmyForBattle (Battlefield attackers defenders) = 
  ((Battlefield (attackers - (availableAttackers attackers)) (defenders - (availableDefenders defenders))), 
  (Battlefield (availableAttackers attackers) (availableDefenders defenders)))

availableAttackers :: Army -> Army
availableAttackers attackers
    | attackers >= 3 = 3
    | otherwise = attackers

availableDefenders :: Army -> Army
availableDefenders defenders
    | defenders >= 2 = 2
    | otherwise = defenders


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
  dies >>= (\(attacker, defender) -> 
    let greatWarriors = (makeSameSize attacker defender) 
        greatAttackers = (\(x,y) -> x) greatWarriors
        greatDefenders = (\(x,y) -> y) greatWarriors
    in return ((length attacker - length greatAttackers) + (compareEveryDieValue (greatWarriors) (>)), (length defender - length greatDefenders) + (compareEveryDieValue (greatWarriors) (<=))))


compareEveryDieValue :: ([DieValue], [DieValue]) -> (DieValue -> DieValue -> Bool) -> Int
compareEveryDieValue (attacker, defender) predicate = sum . concat $ (zipTwoDies attacker defender predicate)


zipTwoDies :: [DieValue] -> [DieValue] -> (DieValue -> DieValue -> Bool) -> [[Int]]
zipTwoDies attacker defender predicate = zipWith (\x y -> [1 | x `predicate` y]) attacker defender

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


diff :: [Int] -> [Int] -> [Int]
diff a b = map (\(p, q) -> p - q) $ zip a b


makeSameSize :: [DieValue] -> [DieValue] -> ([DieValue], [DieValue])
makeSameSize arr1 arr2 
    | length arr1 == length arr2 = (arr1, arr2)
    | length arr1 < length arr2 = (arr1, cropList arr2 (length arr1))
    | length arr1 > length arr2 = (cropList arr1 (length arr2), arr2)

addZeros :: [Int] -> Int -> [Int]
addZeros arr zerosToAdd = arr ++ (take zerosToAdd $ repeat 0)

cropList :: [DieValue] -> Int -> [DieValue]
cropList arr cropTill = take cropTill arr