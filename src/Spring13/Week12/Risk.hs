module Spring13.Week12.Risk 
  ( battle
  ) where

import Control.Monad.Random
import Spring13.Week12.Battlefield
import Spring13.Week12.Dies
import Debug.Trace

------------------------ Invade ------------------------------ 

invade :: Battlefield -> Rand StdGen Battlefield
invade battlefield = 
  (battleWithTrace battlefield) >>= 
    (\resultingBattlefield -> 
      if isBattleOver resultingBattlefield then return (resultingBattlefield) 
      else invade resultingBattlefield)

isBattleOver :: Battlefield -> Bool
isBattleOver battlefield 
  | attackers battlefield <= 1 = True
  | defenders battlefield == 0 = True
  | otherwise = False
  

----------------------- Tracing ----------------------------

battleWithTrace :: Battlefield -> Rand StdGen Battlefield
battleWithTrace battlefield = 
  let battleResult = battle (battlefield) 
  in battleResult >>= (\battleR -> trace (show battleR) return battleR)

-------------------------------------------------------------

battle :: Battlefield -> Rand StdGen Battlefield
battle = executeBattle . splitReserveAndActiveBattlefield

executeBattle :: (Battlefield, Battlefield) -> Rand StdGen Battlefield
executeBattle (reserveBattlefield, fightingBattlefield) = 
  (startWarInActiveBattlefield fightingBattlefield) >>= (\battlefield -> return (mappend battlefield reserveBattlefield))

startWarInActiveBattlefield :: Battlefield -> Rand StdGen Battlefield
startWarInActiveBattlefield (Battlefield attackers defenders) = 
  let sortedAttackerDies = (sortDies . getDieForAttacker) attackers
      sortedDefenderDies = (sortDies . getDieForDefender) defenders
  in getResultOfBattlefield sortedAttackerDies sortedDefenderDies

getResultOfBattlefield :: Rand StdGen [DieValue] -> Rand StdGen [DieValue] -> Rand StdGen Battlefield
getResultOfBattlefield attackerPower defenderPower = 
  (comparePowers attackerPower defenderPower) >>= (\(attackerWon, defenderWon) -> 
    return (Battlefield {attackers = attackerWon, defenders = defenderWon} ))


comparePowers :: Rand StdGen [DieValue] -> Rand StdGen [DieValue] -> Rand StdGen (Army, Army)
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
        winningDefenders = compareEveryDieValue greatAttackers greatDefenders (<=)
        winningAttackers = compareEveryDieValue greatAttackers greatDefenders (>)
    in return (bystandingAttacker + winningAttackers, bystandingDefender + winningDefenders))


compareEveryDieValue :: [DieValue] -> [DieValue] -> (DieValue -> DieValue -> Bool) -> Int
compareEveryDieValue attackers defenders predicate = sum . concat $ zipTwoDies attackers defenders predicate


zipTwoDies :: [DieValue] -> [DieValue] -> (DieValue -> DieValue -> Bool) -> [[Int]]
zipTwoDies attacker defender predicate = zipWith (\x y -> [1 | x `predicate` y]) attacker defender


splitReserveAndActiveBattlefield :: Battlefield -> (Battlefield, Battlefield)
splitReserveAndActiveBattlefield (Battlefield attackers defenders) = 
  let participatingAttackers = availableAttackers attackers
      participatingDefenders = availableDefenders defenders
      restingAttackers = attackers - participatingAttackers
      restingDefenders = defenders - participatingDefenders
    in ((Battlefield restingAttackers restingDefenders), (Battlefield participatingAttackers participatingDefenders))

availableAttackers :: Army -> Army
availableAttackers attackers
    | attackers >= 3 = 3
    | otherwise = attackers

availableDefenders :: Army -> Army
availableDefenders defenders
    | defenders >= 2 = 2
    | otherwise = defenders

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

