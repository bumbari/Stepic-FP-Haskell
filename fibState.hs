--def fib(n):
--  a, b = 0, 1
--  for i in [1 .. n]:
--    a, b = b, a + b
--  return a

import Control.Monad.State

fibStep :: State (Integer, Integer) ()
fibStep = do
	(a,b) <- get
	put (b,a+b)

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState $ replicateM n m

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)

main :: IO ()
main = do
	print $ execState fibStep (0,1)
	print $ execState fibStep (1,1)
	print $ execState fibStep (1,2)
	print $ execStateN 5 fibStep (0, 1)
	print $ fib 5
	print $ execStateN 10 fibStep (0, 1)
	print $ fib 10