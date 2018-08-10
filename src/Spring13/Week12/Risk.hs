{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Spring13.Week12.Risk 
  ( battle
  ) where

import Control.Monad.Random
import Spring13.Week12.Battlefield
import Spring13.Week12.Dies
import Debug.Trace


----------------------- Probability -------------------------

instance Monoid (Rand StdGen Double) where
  mempty = return 0.0
  mappend battle1 battle2 = battle1 >>= (\b1 -> battle2 >>= (\b2 -> return (b1 + b2)))


successProb :: Battlefield -> Rand StdGen Double
successProb = (fmap (/1000.0)). attackerWinningCount . (replicate 1000) . invade


attackerWinningCount :: [Rand StdGen Battlefield] -> Rand StdGen Double
attackerWinningCount = mconcat . map (fmap attackerWon)

attackerWon :: Battlefield -> Double
attackerWon (Battlefield 1 _) = 0
attackerWon (Battlefield _ 0) = 1


------------------------ Invade ------------------------------ 
-- Does not handle attackers with 0 and 1
--------------------------------------------------------------

traceStarting :: Battlefield -> String
traceStarting battlefield = 
  "-> Starting Battle with::  " ++ show battlefield ++ "\n" ++
    "kept 1 army as picket \n"

invade :: Battlefield -> Rand StdGen Battlefield
invade = startBattle . reserveOneAttackingArmy

startBattle :: (Army, Battlefield) -> Rand StdGen Battlefield
startBattle (picket, battlefield) = 
  let goForBattleWithTrace = trace (traceStarting battlefield) goForBattle battlefield
  in (goForBattleWithTrace) >>= (\battlefield -> return (addPicketToBattleField picket battlefield))

goForBattle :: Battlefield -> Rand StdGen Battlefield
goForBattle battlefield = (battleWithTrace battlefield) >>= 
  (\resultingBattlefield -> 
    if isBattleOver resultingBattlefield then return resultingBattlefield
    else goForBattle resultingBattlefield)

addPicketToBattleField :: Army -> Battlefield -> Battlefield
addPicketToBattleField picket (Battlefield attackers defenders) = Battlefield (attackers + picket) (defenders)

reserveOneAttackingArmy :: Battlefield -> (Army, Battlefield)
reserveOneAttackingArmy (Battlefield attackers defenders) = (1, Battlefield (attackers - 1) (defenders))

isBattleOver :: Battlefield -> Bool
isBattleOver (Battlefield 0 _) = True
isBattleOver (Battlefield _ 0) = True
isBattleOver (Battlefield _ _) = False

----------------------- Tracing ----------------------------

battleWithTrace :: Battlefield -> Rand StdGen Battlefield
battleWithTrace battlefield = 
  let battleResult = battle (battlefield) 
  in battleResult >>= (\battleR -> trace ("Running Battle :: " ++ show battleR) return battleR)

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

