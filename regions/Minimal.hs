{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds, KindSignatures, MultiParamTypeClasses
             , TypeFamilies, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE OverlappingInstances #-}

import Control.Applicative
import Control.Monad.Reader
import Data.Type.Equality
import Data.Type.Bool
-- import Data.Proxy

import Debug.Trace

newtype IORT s m v = IORT{ unIORT:: ReaderT () m v } 
    deriving (Functor, Applicative, Monad, MonadIO)

newtype SHandle (m :: * -> *) = SHandle ()

class (Monad m1, Monad m2) => MonadRaise m1 m2 where
  lifts :: m1 a -> m2 a

-- closed type families based solution (doesn't work)
------------------------------------------------------

{-
type family TEQ (a :: * -> *) (b :: * -> *) :: Bool where
  TEQ m1 (IORT s m2) = False
  TEQ m  m2  = (m == m2)
-}

data Proxy (b::Bool) = Proxy

data MProxy (m:: * -> *) = MProxy

class (Monad m1, Monad m2) => MonadRaise' (b::Bool) m1 m2 where
  lifts' :: Proxy b -> m1 a -> m2 a

instance (MonadRaise' (TEQ m1 m2) m1 m2) => MonadRaise m1 m2 where
  lifts = lifts' (Proxy::Proxy (TEQ m1 m2))

instance (Monad m1, Monad m2, m1 ~ m2) => MonadRaise' True m1 m2 where
  lifts' _ = id

instance (Monad m2, m2 ~ (IORT s m2'), MonadRaise m1 m2')
  => MonadRaise' False m1 m2 where
  lifts' _ = IORT . trace "up" . lift . lifts


-- Approach 1 just listify our type and try to perform some 
-- operations on that form

type family Listify (a :: * -> *) :: [* -> *] where
   Listify (IORT s m)  = IORT s m ': Listify m
   Listify m          = '[m]

type family TEQ' (a:: [* -> *]) (b::[* -> *]) :: Bool where
--   TEQ' (IO ': as) (IO ': bs) = TEQ' as bs
   TEQ' (IORT s  a ': as) (IORT s b ': bs) = True
   TEQ' (IORT p  a ': as) (IORT s b ': bs) = False && TEQ' (IORT p a ': as) bs
--   TEQ' '[]  (m ': ms)    = False
   TEQ' '[]  '[] = True 

type TEQ (a:: * -> *) (b :: * -> *) = TEQ' (Listify a) (Listify b)

{-
data O a = O (O a) a
         | Z a

type family Listify' (a :: * -> *) :: O (* -> *) where
   Listify' IO         = 'Z IO
   Listify' (IORT s m) = 'O (Listify' m)  (IORT s m)
   Listify' m          = 'Z m

type family TEQ' (a :: O (* -> *)) (b :: O (* -> *)) :: Bool where
   TEQ' ('O a b) ('O c d) = False
   -- TEQ' ('O a c) ('O b c) = True
   -- TEQ' ('Z a)   ('Z a)   = True
   -- TEQ' ('O a b) ('O a c) = TEQ' ('Z b) ('Z c)
 

type TEQ (a:: * -> *) (b :: * -> *) = TEQ' (Listify' a) (Listify' b)
-}

{-
type Reverse x = Reverse' '[] x

type family Reverse' (acc::[* -> *]) (x::[* -> *]) :: [* -> *] where
   Reverse' a '[] = a
   Reverse' a (x ': xs) = Reverse' (x ': a) xs

type family TEQ' (a:: [* -> *]) (b::[* -> *]) :: Bool where
--   TEQ' (IO ': as) (IO ': bs) = TEQ' as bs
--   TEQ' (IORT s a ': as) (IORT s b ': bs) = TEQ' as bs
--   TEQ' '[]  (m ': ms)    = False
--   TEQ' '[]  '[] = True 

type TEQ (a :: * -> *) (b :: * -> *) = TEQ' (Reverse (Listify a)) (Reverse (Listify b))
-}

{-
type family TEQ'' (a :: [* -> *]) (b :: [* -> *]) :: Bool where
   TEQ'' (IORT s  a ': as) (IORT s b ': bs) = True  && EqTail as bs -- if S variable is equal
   TEQ'' (IORT s' a ': as) (IORT s b ': bs) = False && TEQ'' (IORT s' a ': as) bs

type family EqTail (a :: [* -> *]) (b :: [* -> *]) :: Bool where -- could be :: Constraint
   EqTail (IORT s a ': as) (IORT s b ': bs) = EqTail as bs
   EqTail (IO ': as)       (IO ': bs)       = EqTail as bs
   EqTail '[]              '[]              = True

type family TEQ'' (a :: [* -> *]) (b :: [* -> *]) :: Bool where
   TEQ'' a         (b ': a )                = False -- capture tail
   TEQ'' (a ': as) (a ': bs)                = True
   TEQ'' a         (IORT s m ': bs)         = False
   -- TEQ'' (IORT s' a ': as) (IORT s b ': bs) = False && TEQ'' (IORT s' a ': as) bs

type TEQ (a :: * -> *) (b :: * -> *) = TEQ'' (Listify a) (Listify b)
-}

{-
type family TEQ''' (a :: [* -> *]) (b :: [* -> *]) :: Bool where
   -- TEQ''' (IORT s a ': as) (IORT s  b ': bs) = True
   TEQ''' (IORT s a ': as) (IORT s' b ': bs) = If (s == s') True False 
   TEQ''' '[] '[] = True

-- type TEQ (a :: * -> *) (b :: * -> *) = TEQ''' (Listify a) (Listify b)
-}

{-
data V 

type family Head (a :: * -> *) where
   Head (IORT s m) = s
   Head IO         = V
 
-- type TEQ (a :: * -> *) (b :: * -> *) = Head a == Head b
-}

-- overlapping instances based solutio  (works)
-----------------------------------------------

{-
instance Monad m => MonadRaise m m
  where lifts = trace "m=m" id

instance (Monad m2, m2 ~ (IORT s m2'), MonadRaise m1 m2')
  => MonadRaise m1 m2 where
    lifts = IORT . trace "up" . lift . lifts -- . lift . lifts
-}
-- Test that doesn't work
--------------------------------------------------------------------
{-
test_2 :: IORT s IO ()
test_2 = do
  hout <- newSHandle
  shPutStrLn hout
  -}

test_copy :: forall s' (m' :: * -> *). MonadIO m' => IORT s' m' ()
test_copy = do
  hout <- newSHandle
  hp <- mkProxy :: IORT s' m' (MProxy (IORT s' m'))
  newRgn $ do
    p <- mkProxy 
    shps hp p hout
  where 
    shps :: (MonadRaise (IORT s' m') p) => proxy (IORT s' m') -> proxy p -> SHandle (IORT s' m') -> p ()
    shps _ _ = shPutStrLn

runSIO :: (forall s. IORT s IO v) -> IO v
runSIO = newRgn


-------------------------------------------------------------------

newSHandle :: (m ~ (IORT s' m'), MonadIO m) => m (SHandle m)
newSHandle = return $ SHandle ()

newRgn :: MonadIO m => (forall s. IORT s m v) -> m v
newRgn (IORT x) = runReaderT x () 

mkProxy :: Monad m1 => m1 (MProxy m1)
mkProxy = return $ MProxy

shPutStrLn :: forall m1 m2 . (MonadRaise m1 m2, MonadIO m1) => SHandle m1 -> m2 ()
shPutStrLn _ = lifts $ r
  where 
    r :: m1 ()
    r = liftIO $ putStrLn "hi"
