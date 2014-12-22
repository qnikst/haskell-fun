{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds, KindSignatures, MultiParamTypeClasses
             , TypeFamilies, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

{-# LANGUAGE OverlappingInstances #-}

import Control.Applicative
import Control.Monad.Reader

newtype IORT s m v = IORT{ unIORT:: ReaderT () m v } 
    deriving (Functor, Applicative, Monad, MonadIO)

newtype SHandle (m :: * -> *) = SHandle ()

class (Monad m1, Monad m2) => MonadRaise m1 m2 where
  lifts :: m1 a -> m2 a

-- closed type families based solution (doesn't work)
------------------------------------------------------

type family TEQ (a :: * -> *) (b :: * -> *) :: Bool where
  TEQ m  m  = True
  TEQ m1 (IORT s m2) = False

data Proxy (b::Bool) = Proxy

class (Monad m1, Monad m2) => MonadRaise' (b::Bool) m1 m2 where
  lifts' :: Proxy b -> m1 a -> m2 a

instance (MonadRaise' (TEQ m1 m2) m1 m2) => MonadRaise m1 m2 where
  lifts = lifts' (Proxy::Proxy (TEQ m1 m2))

instance (Monad m1, Monad m2, m1 ~ m2) => MonadRaise' True m1 m2 where
  lifts' _ = id

instance (Monad m2, m2 ~ (IORT s m2'), MonadRaise m1 m2')
  => MonadRaise' False m1 m2 where
  lifts' _ = IORT . lift . lifts

-- overlapping instances based solutio  (works)
-----------------------------------------------

{-
instance Monad m => MonadRaise m m
  where lifts = id

instance (Monad m2, m2 ~ (IORT s m2'), MonadRaise m1 m2')
  => MonadRaise m1 m2 where
    lifts = IORT . lift . lifts
-}

-- Test that doesn't work
--------------------------------------------------------------------
test_copy :: forall s' (m' :: * -> *). MonadIO m' => IORT s' m' ()
test_copy = do
  hout <- newSHandle
  newRgn $ shPutStrLn hout

-------------------------------------------------------------------

newSHandle :: (m ~ (IORT s' m'), MonadIO m) => m (SHandle m)
newSHandle = undefined

newRgn :: MonadIO m => (forall s. IORT s m v) -> m v
newRgn = undefined 

shPutStrLn :: (MonadRaise m1 m2, MonadIO m2) => SHandle m1 -> m2 ()
shPutStrLn = undefined
