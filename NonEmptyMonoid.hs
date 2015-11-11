import Data.Monoid

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)
    mappend (Maybe' Nothing) _ = Maybe' Nothing
    mappend _ (Maybe' Nothing) = Maybe' Nothing
    mappend (Maybe' a) (Maybe' b) = Maybe' $ a `mappend` b

no = (Maybe' { getMaybe = (Nothing :: Maybe [Int])})
a = (Maybe' {getMaybe = Just ([] :: [Int])})
b = (Maybe' {getMaybe = Just ([1,2,3] ::[Int])})
c = (Maybe' {getMaybe = Just ([2,1,5] ::[Int])})
d = (Maybe' {getMaybe = Just ([10,11,12,22] ::[Int])})

no' = (Maybe' { getMaybe = (Nothing :: Maybe (Sum Int))})
a' = (Maybe' {getMaybe = Just (Sum 0 :: Sum Int)})
b' = (Maybe' {getMaybe = Just (Sum 22 :: Sum Int)})
c' = (Maybe' {getMaybe = Just (Sum 32 :: Sum Int)})
d' = (Maybe' {getMaybe = Just (Sum (-15) :: Sum Int)})


noInner = (Maybe' { getMaybe = (Nothing :: (Maybe (Maybe' [Int])))})
aInner = (Maybe' {getMaybe = Just ((Maybe' {getMaybe = Just ([] :: [Int])}))})
bInner = (Maybe' {getMaybe = Just (Maybe' {getMaybe = Just ([1,2,3] ::[Int])})})
cInner = (Maybe' {getMaybe = Just (Maybe' {getMaybe = Just ([2,1,5] ::[Int])})})
dInner = (Maybe' {getMaybe = Just (Maybe' {getMaybe = Just ([10,11,12,22] ::[Int])})})

test1 = filter f list where
    list = [ (x,y,z, zz, ((applyLeft x y z zz) == (applyRight x y z zz))) | x <- values, y <- values, z <- values, zz <- values ]
    values = [a,b,no,c,d]
    applyLeft x y z zz =  (mappend x (mappend y (mappend z zz)))
    applyRight x y z zz = (mappend (mappend (mappend x y) z) zz)  
    f (x,y,z,zz,flag) = flag == False 

test2 = filter f list where
    list = [ (x,y,z, ((applyLeft x y z) == (applyRight x y z))) | x <- values, y <- values, z <- values ]
    values = [a,b,no,c,d]
    applyLeft x y z =  (mappend x (mappend y z))
    applyRight x y z = (mappend (mappend x y) z)  
    f (x,y,z,flag) = flag == False   

testSum1 = filter f list where
    list = [ (x,y,z, zz, ((applyLeft x y z zz) == (applyRight x y z zz))) | x <- values, y <- values, z <- values, zz <- values ]
    values = [a',b',no',c',d']
    applyLeft x y z zz =  (mappend x (mappend y (mappend z zz)))
    applyRight x y z zz = (mappend (mappend (mappend x y) z) zz)  
    f (x,y,z,zz,flag) = flag == False 
  
  
testSum2 = filter f list where
    list = [ (x,y,z, ((applyLeft x y z) == (applyRight x y z))) | x <- values, y <- values, z <- values ]
    values = [a',b',no',c',d']
    applyLeft x y z =  (mappend x (mappend y z))
    applyRight x y z = (mappend (mappend x y) z)  
    f (x,y,z,flag) = flag == False     

  
testInner1 = filter f list where
    list = [ (x,y,z, zz, ((applyLeft x y z zz) == (applyRight x y z zz))) | x <- values, y <- values, z <- values, zz <- values ]
    values = [aInner,bInner,noInner,cInner,dInner]
    applyLeft x y z zz =  (mappend x (mappend y (mappend z zz)))
    applyRight x y z zz = (mappend (mappend (mappend x y) z) zz)  
    f (x,y,z,zz,flag) = flag == False 
  
  
testInner2 = filter f list where
    list = [ (x,y,z, ((applyLeft x y z) == (applyRight x y z))) | x <- values, y <- values, z <- values ]
    values = [aInner,bInner,noInner,cInner,dInner]
    applyLeft x y z =  (mappend x (mappend y z))
    applyRight x y z = (mappend (mappend x y) z)  
    f (x,y,z,flag) = flag == False