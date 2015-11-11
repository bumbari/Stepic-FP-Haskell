import Control.Monad.Reader
import Control.Monad.State

readerToState :: Reader r a -> State r a
readerToState m = state $ \st -> (runReader m st, st)

main :: IO ()
main = do
	print $ evalState (readerToState $ asks (+2)) 4
	print $ runState (readerToState $ asks (+2)) 4