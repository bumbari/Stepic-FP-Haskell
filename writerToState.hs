import Control.Monad.Writer
import Control.Monad.State

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = state $ \s' -> (a, s' `mappend` s)
  where (a,s) = runWriter m

main :: IO ()
main = do
	print $ runState (writerToState $ tell "world") "hello,"
	print $ runState (writerToState $ tell "world") mempty