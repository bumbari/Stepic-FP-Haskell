import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
	empty = ListMap []
	lookup key (ListMap []) = Nothing
	lookup key (ListMap ((k,v):xs)) | key == k  = Just v 
	lookup key (ListMap (_:xs))                 = lookup key (ListMap xs)
	insert key val lm@(ListMap m) = ListMap $ (key,val) : getListMap (delete key lm)
	delete key (ListMap m) = ListMap $ filter (\(k, _) -> (k /= key)) m