{-# OPTIONS_GHC -Wall #-}
module Spring13.Week4.Four
  ( sieveSundaram
  ) where

import Data.List

sieveSundaram :: Integer -> [Integer]
sieveSundaram = doubleAndIncrementBy1 . removeNumbersBasedOnFormula

doubleAndIncrementBy1 :: [Integer] -> [Integer]
doubleAndIncrementBy1 = map (\x -> (2 * x) + 1)

removeNumbersBasedOnFormula :: Integer -> [Integer]
removeNumbersBasedOnFormula number = [1..number] \\ (applyFormula number)

applyFormula :: Integer -> [Integer]
applyFormula number = [ i + j + (2 * i * j) | j <- [1..number], i <- [1..j], (i + j + (2 * i * j) <=number) ]