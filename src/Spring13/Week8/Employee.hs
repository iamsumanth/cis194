module Spring13.Week8.Employee where

import Data.Tree

-- Employee names are represented by Strings.
type Name = String

-- The amount of fun an employee would have at the party, represented
-- by an Integer
type Fun  = Integer

-- An Employee consists of a name and a fun score.
data Employee = Emp { empName :: Name, empFun :: Fun }
  deriving (Show, Read, Eq)

sampleGuestList1 :: GuestList
sampleGuestList1 = GL [(Emp "Stan" 9), (Emp "Bob" 2), (Emp "Joe" 5), (Emp "John" 1)] 17

sampleGuestList2 :: GuestList
sampleGuestList2 = GL [(Emp "Sue" 5), (Emp "Fred" 3), (Emp "Sarah" 17), (Emp "Sam" 4)] 29

sampleGuestList3 :: GuestList
sampleGuestList3 = GL [(Emp "Sue" 5), (Emp "Fred" 3), (Emp "Sarah" 17)] 25

sampleGuestList4 :: GuestList
sampleGuestList4 = GL [(Emp "Fred" 3), (Emp "Sarah" 17)] 20

-- A small company hierarchy to use for testing purposes.
testCompany :: Tree Employee
testCompany
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 2)
      [ Node (Emp "Joe" 5)
        [ Node (Emp "John" 1) []
        , Node (Emp "Sue" 5) []
        ]
      , Node (Emp "Fred" 3) []
      ]
    , Node (Emp "Sarah" 17)
      [ Node (Emp "Sam" 4) []
      ]
    ]

testCompany2 :: Tree Employee
testCompany2
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 3) -- (8, 8)
      [ Node (Emp "Joe" 5) -- (5, 6)
        [ Node (Emp "John" 1) [] -- (1, 0)
        , Node (Emp "Sue" 5) [] -- (5, 0)
        ]
      , Node (Emp "Fred" 3) [] -- (3, 0)
      ]
    , Node (Emp "Sarah" 17) -- (17, 4)
      [ Node (Emp "Sam" 4) [] -- (4, 0)
      ]
    ]

-- A type to store a list of guests and their total fun score.
data GuestList = GL [Employee] Fun
  deriving (Show, Eq)

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2
