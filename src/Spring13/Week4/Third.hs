{-# OPTIONS_GHC -Wall #-}
module Spring13.Week4.Third
  ( xor
  ) where

xor :: [Bool] -> Bool
xor booleanList = foldr (flipIfTrue) False booleanList

flipIfTrue :: Bool -> Bool -> Bool
flipIfTrue output input = if input then not output else output


map' :: (a -> b) -> [a] -> [b]
map' func inputs = foldr (\input acc -> applyFunc func acc input) [] inputs

applyFunc :: (a -> b) -> [b] -> a -> [b]
applyFunc func acc input = [func input] ++ acc

