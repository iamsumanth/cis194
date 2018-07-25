module Spring13.Week8.Party
  (
  ) where
import Spring13.Week8.Employee
import System.Environment  
import System.IO  
import System.IO.Error

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL employees1 funScore1) (GL employees2 funScore2) = GL (employees1 ++ employees2) (funScore1 + funScore2)


glCons :: Employee -> GuestList -> GuestList
glCons (employee) (GL employees funScore) = GL (employees ++ [employee]) (funScore + empFun employee)


moreFun :: GuestList -> GuestList -> GuestList
moreFun guestList1 guestList2
    | guestList1 > guestList2 = guestList1
    | otherwise = guestList2

   
treeFold :: (a -> b) -> Tree a -> b
treeFold func node = foldl foldTree node