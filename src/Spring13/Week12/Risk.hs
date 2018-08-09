module Spring13.Week12.Risk 
  ( DieValue
  ) where

import Control.Monad.Random
import Spring13.Week12.Battlefield
import Spring13.Week12.Dies

------------------------------------------------------------
-- Risk


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
        bystandingAttacker = length attacker - length greatAttackers
        bystandingDefender = length defender - length greatDefenders
        saviorsOfLand = compareEveryDieValue greatAttackers greatDefenders (<=)
    in return (bystandingAttacker + (compareEveryDieValue greatAttackers greatDefenders (>)), bystandingDefender + saviorsOfLand))


compareEveryDieValue :: [DieValue] -> [DieValue] -> (DieValue -> DieValue -> Bool) -> Int
compareEveryDieValue attackers defenders predicate = sum . concat $ zipTwoDies attackers defenders predicate


--------------------------------------------------- Risk

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

