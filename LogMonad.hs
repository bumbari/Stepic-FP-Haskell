data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg x = Log [msg] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (l1 ++ l2) c
  where
  	(Log l1 b) = f x
  	(Log l2 c) = g b

add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog l f = Log (ls2 ++ ls1 ++ ls2) b
  where
    (Log ls1 a) = l
    (Log ls2 b) = f a

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList x = foldl bindLog (return x)
	--helper (return x)
	--where
		--helper a [] = a
		--helper a (f:fs) = helper (bindLog a f) fs
		--helper = foldl bindLog