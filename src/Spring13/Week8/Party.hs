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
glCons (employee) (GL employees funScore) = if (elem employee employees) then (GL employees funScore) else GL (employees ++ [employee]) (funScore + empFun employee)


moreFun :: GuestList -> GuestList -> GuestList
moreFun guestList1 guestList2
    | guestList1 > guestList2 = guestList1
    | otherwise = guestList2


nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel employee subTrees = ((getBestGuestList employee (extractWithBoss subTrees)), (getBestGuestList employee (extractWithOutBoss subTrees)))

extractWithBoss :: [(GuestList, GuestList)] -> [GuestList]
extractWithBoss guestLists = [withBoss | (withBoss, _) <- guestLists]

extractWithOutBoss :: [(GuestList, GuestList)] -> [GuestList]
extractWithOutBoss guestLists = [withOutBoss | (_, withOutBoss) <- guestLists]

getBestGuestList :: Employee -> [GuestList] -> GuestList
getBestGuestList  employee [] = mempty
getBestGuestList (employee) (guestList1:[]) = glCons employee guestList1
getBestGuestList (employee) (guestList1:guestList2:[]) = moreFun (glCons employee guestList1) (glCons employee guestList2)
getBestGuestList (employee) (guestList1:guestList2:rest) = getBestGuestList employee ([(moreFun (glCons employee guestList1) (glCons employee guestList2))] ++ rest)