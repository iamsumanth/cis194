module Spring13.Week7.Scrabble
  ( Score, scoreString

  ) where
import qualified Data.HashMap.Strict as M

newtype Score = Score Int
  deriving (Eq, Ord, Show)


instance Monoid Score where
  mempty = Score 0
  mappend (Score a) (Score b)= Score (a + b)

alphabets = ['a'..'z']
scrabbleNumbers = [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]
scrabbleMap = M.fromList (zip alphabets scrabbleNumbers)

score :: Char -> Score
score char = Score (getInt (M.lookup char scrabbleMap))

scoreString :: String -> Score
scoreString str = mconcat [score x | x <- str]


getInt :: Maybe Int -> Int
getInt (Nothing) = 0
getInt (Just value) = value
