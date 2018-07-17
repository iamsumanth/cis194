{-# LANGUAGE ScopedTypeVariables #-}
module Spring13.Week7.JoinList
  (

  ) where

import Spring13.Week7.Sized
import Spring13.Week7.Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- instance Monoid (JoinList m a) where
--   mempty = Empty
--   mappend () () = ()


data Product a = Product a
  deriving (Eq, Ord, Show)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend (Product val1) (Product val2) = Product (val1 * val2)

-- (+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
-- (+++) (Empty) (Empty) = Empty
-- (+++) (Empty) (Single m a) = Single m a
-- (+++) (Single m a) (Empty) = Single m a
-- (+++) (Single m1 a1) (Single m2 a2) = Append (m1 `mappend` m2) (Single m1 a1) (Single m2 a2)
-- (+++) (Single m1 a1) (Append m2 (l1) (l2)) = Append (m1 `mappend` m2) (Single m1 a1) (Append m2 (l1) (l2))
-- (+++) (Append m1 (l1)(l2)) (Single m2 a2) = Append (m1 `mappend` m2) (Append m1 (l1) (l2)) (Single m2 a2)
-- (+++) (Append m1 (l1)(l2)) (Append m2 (l3)(l4))) = Append (m1 `mappend` m2) (Append m1 (l1)(l2)) (Append m2 (l3)(l4)))
-- (+++) (Empty) (Append m (l1)(l2)) = Append (m) (Empty) (Append m l1 l2)
-- (+++) (Append m (l1)(l2)) (Empty) = Append (m) (Append m l1 l2) (Empty)


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) (l1) (l2) = Append (tag l1 `mappend` tag l2) l1 l2


tag :: Monoid m => JoinList m a -> m
tag (Single m a) = m
tag (Append m l1 l2) = m


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ index Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ index (Append (sized) (l1) (l2))
    | (index >= (getSize (size sized))) = Nothing
    | isLeft index l1 == True = indexJ index l1
    | otherwise = indexJ (index - (getSizeOfList l1)) l2

getSizeOfList :: (Sized b, Monoid b) => JoinList b a -> Int
getSizeOfList (Empty) = 0
getSizeOfList (Single _ _) = 1
getSizeOfList (Append (sized) _ _) = getSize (size sized)

isLeft :: (Sized b, Monoid b) => Int -> JoinList b a -> Bool
isLeft _ Empty = False
isLeft index (Single sized a) = if index ==0 then True else False
isLeft index (Append (sized) _ _) = if (index < (getSize (size sized))) then True else False



scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str
