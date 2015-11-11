import Control.Monad.Writer

type Shopping = Writer [(Integer,String)] ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

purchase :: String -> Integer -> Shopping
purchase item cost = tell [(cost,item)]

total :: Shopping -> Integer
total = sum . map fst . execWriter

items :: Shopping -> [String]
items = map snd . execWriter

main :: IO ()
main = do
	print $ runWriter shopping1
	print $ total shopping1
	print $ items shopping1
