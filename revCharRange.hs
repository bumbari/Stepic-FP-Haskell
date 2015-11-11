import Data.List

revRange :: (Char,Char) -> [Char]
revRange (fc,sc) = unfoldr g sc
  where
  	g x | x >= fc && x <= sc = Just(x, pred x)
  		| otherwise          = Nothing