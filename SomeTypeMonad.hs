import Control.Monad

data SomeType a = Some a

instance Monad SomeType where
	--return :: a -> m a
	return = Some
	--(>>=) :: m a -> (a -> m b) -> m b
	Some x >>= f = f x

instance Functor SomeType where
    --fmap :: (a -> b) -> f a -> f b
    --fmap    = liftM
    --fmap f m1 = do { x1 <- m1; return (f x1) }
    fmap f x   = x >>= \a -> return (f a)