import Control.Monad

data Board = Board Int deriving (Show, Eq)

nextPositions :: Board -> [Board]
nextPositions (Board x) = map Board [x-1,x+1]

{-
nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b 0 pred | pred b = [b] 
nextPositionsN _ 0 _             = [] 
nextPositionsN b n pred = 
		do
			p <- nextPositions b
			p : filter (\x -> pred x && p /= x) (nextPositionsN p (n - 1) pred)
-}

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b 0 pred | pred b = [b]
nextPositionsN _ n _    | n <= 0  = []
nextPositionsN b n pred = do
	p <- nextPositions b
	nextPositionsN p (n-1) pred

f (Board x) = x > 3
fe (Board x) = even x