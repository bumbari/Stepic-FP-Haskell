import           Control.Monad.State

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

numbering :: Tree () -> State Integer (Tree Integer)
numbering (Leaf _) = do
	n <- get
	put (n+1)
	return (Leaf n)
numbering (Fork l _ r) = do
	left <- numbering l
	n <- get
	put (n+1)
	right <- numbering r
	return (Fork left n right)

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (numbering tree) 1

numberTree' :: Tree () -> Tree Integer
numberTree' tree = snd $ helper 1 tree
  where
  	helper n (Leaf ()) = (n+1, Leaf n)
  	helper n (Fork lt _ rt) = (y+1, Fork left x rigth)
  	  where
  	  	(x, left) = helper n lt
  	  	(y, rigth) = helper (x+1) rt

main :: IO ()
main = do
	print $ numberTree (Leaf ())
	print $ numberTree' (Leaf ())
	print $ numberTree (Fork (Leaf ()) () (Leaf ()))
	print $ numberTree' (Fork (Leaf ()) () (Leaf ()))
