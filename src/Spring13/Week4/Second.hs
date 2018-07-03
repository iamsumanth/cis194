{-# OPTIONS_GHC -Wall #-}
module Spring13.Week4.Second
  ( foldTree
  ) where

--WIP

data Tree = Leaf
          | Node Integer Tree Char Tree
  deriving (Show, Eq)

data TreeState = Balanced | LeftMore | RightMore

foldTree :: String -> Tree
foldTree [] = Leaf
foldTree (first:[]) = Node 0 Leaf first Leaf
foldTree (first:rest) = foldl (insert) (Node 0 Leaf first Leaf) rest

-- if (isBalanced leftTree rightTree) then (insert leftTree inputNodeValue) else (insert rightTree inputNodeValue)
-- insert (Node 1 leftTree currentNodeValue Leaf) inputNodeValue = Node (1) leftTree currentNodeValue (Node 0 Leaf inputNodeValue Leaf)
insert :: Tree -> Char -> Tree
insert Leaf inputNodeValue = Node 0 Leaf inputNodeValue Leaf
insert (Node 0 Leaf currentNodeValue Leaf) inputNodeValue = Node (1) (Node 0 Leaf inputNodeValue Leaf) currentNodeValue Leaf 
insert (Node currentNodeHeight leftTree currentNodeValue rightTree) inputNodeValue 
  | (isBalanced leftTree rightTree) == 1 = Node (currentNodeHeight) leftTree currentNodeValue (insert rightTree inputNodeValue)
  | (isBalanced leftTree rightTree) == 0 = Node (currentNodeHeight) (insert leftTree inputNodeValue) currentNodeValue rightTree
  | (isBalanced leftTree rightTree) == -1 = Node (currentNodeHeight) (insert leftTree inputNodeValue) currentNodeValue rightTree

isBalanced :: Tree -> Tree -> Integer
isBalanced Leaf Leaf = 0
isBalanced Leaf _ = -1
isBalanced _ Leaf = 1
isBalanced (Node leftHeight _ _ _) (Node rightHeight _ _ _) = leftHeight - rightHeight