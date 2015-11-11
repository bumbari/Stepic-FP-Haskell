data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg x = Log [msg] (f x)

add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (l1 ++ l2) c
  where
  	(Log l1 b) = f x
  	(Log l2 c) = g b