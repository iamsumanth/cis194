module Spring13.Week1.CreditCard
    ( validate
    ) where

validate :: Integer -> Bool
validate cardNumber = isDividedBy10 (sumDigits (doubleEveryOther (toDigits cardNumber)))

toDigits :: Integer -> [Integer]
toDigits cardNumber
    | cardNumber <=0 = []
    | otherwise = splitNumber cardNumber

toDigitsRev :: Integer -> [Integer]
toDigitsRev cardNumber = reverseDigits (toDigits cardNumber)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther digits = reverseDigits (doubleEveryOtherFromFirst (reverseDigits digits))

sumDigits :: [Integer] -> Integer
sumDigits digits = sumList (splitDigits digits)



isDividedBy10 :: Integer -> Bool
isDividedBy10 number
    | (number `mod` 10 == 0) = True
    | otherwise = False

sumList :: [Integer] -> Integer
sumList (first:[]) = first
sumList (first:second:[]) = first + second
sumList (first:rest) = first + sumList rest

splitDigits :: [Integer] -> [Integer]
splitDigits (first:[]) = splitNumber first
splitDigits (first:rest) = splitNumber first ++ splitDigits rest

doubleEveryOtherFromFirst :: [Integer] -> [Integer]
doubleEveryOtherFromFirst (first:[]) = [first]
doubleEveryOtherFromFirst (first:second:[]) = [first, second * 2]
doubleEveryOtherFromFirst (first:second:rest) = [first, second * 2] ++ (doubleEveryOtherFromFirst rest)

splitNumber :: Integer -> [Integer]
splitNumber cardNumber
    | (cardNumber < 10) = [cardNumber]
    | otherwise = (splitNumber (cardNumber `div` 10)) ++ [cardNumber `mod` 10]

reverseDigits :: [Integer] -> [Integer]
reverseDigits [] = []
reverseDigits (first:[]) = [first]
reverseDigits (first:second:[]) = [second, first]
reverseDigits (first:rest) = (reverseDigits rest) ++ [first]
