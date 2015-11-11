import Control.Monad.Writer

type Shopping = Writer (Sum Integer) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

purchase :: String -> Integer -> Shopping
purchase _ cost = tell $ Sum cost

total :: Shopping -> Integer
total = getSum . execWriter

main :: IO ()
main = do
	print $ runWriter shopping1
	print $ total shopping1