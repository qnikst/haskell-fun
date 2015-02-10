{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

import           Control.Applicative
import           Data.Bifunctor (second)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import           Control.Concurrent
import           System.IO.Unsafe
import           Data.Typeable
import           Data.Proxy

-- Typed device index
class MkDevice a where
  mkDevice :: Cthulhu s -> Int -> SIO s a

data family DeviceIndex a :: * 
class HaveIndex a where getIndex :: a -> Int
-- Hack, because we can't create Typeable instances for data families:
class ToDevRep a where 
  type Rep a :: *
  toDevRep :: a -> Proxy (Rep a)
  toDevRep _ = Proxy
instance ToDevRep a => ToDevRep (Proxy a) where
    type Rep (Proxy a) = a

-- Devices
data DeviceA s = DeviceA {
        deviceAI :: Int
        } deriving Typeable
instance MkDevice (DeviceA s) where
  mkDevice c i = fmap DeviceA (callToCthulhu c i)
newtype instance DeviceIndex (DeviceA s) = IA Int
instance HaveIndex (DeviceIndex (DeviceA s)) where getIndex (IA i) = i
data DeviceARep = DeviceARep deriving Typeable
instance ToDevRep (DeviceIndex (DeviceA s)) where 
    type Rep (DeviceIndex (DeviceA s)) = DeviceARep

data DeviceB s = DeviceB {
        deviceBI :: String
        } deriving Typeable
instance MkDevice (DeviceB s) where
  mkDevice c i = fmap DeviceB (callToCthulhu c (show i))
newtype instance DeviceIndex (DeviceB s) = IB Int
instance HaveIndex (DeviceIndex (DeviceB s)) where getIndex (IB i) = i
data DeviceBRep = DeviceBRep deriving Typeable
instance ToDevRep (DeviceIndex (DeviceB s)) where
    type Rep (DeviceIndex (DeviceB s)) = DeviceBRep

data Cthulhu s = Cthulhu -- not exported

callToCthulhu :: Cthulhu s -> a -> SIO s a
callToCthulhu _ a = return a

type XXX g s = (ToDevRep (DeviceIndex (g s)), Typeable (Rep (DeviceIndex (g s))), HaveIndex (DeviceIndex (g s)), MkDevice (g s))

data SomeIndex = forall a . Typeable a => SomeIndex a

newtype SIO s a = SIO {runSIO :: IO a} deriving (Functor, Applicative, Monad)


withDevice :: XXX g s => DeviceIndex (g s) -> (forall s . g s -> SIO s b) -> IO b
withDevice d f = registering d $ \mk -> runSIO $ mk >>= f

registering :: XXX g s => DeviceIndex (g s) -> (SIO s (g s) -> IO b) -> IO b
registering d f =
    case mv of
      Nothing -> error "register: no such device"
      Just mtx  -> 
        withMVar mtx $ \(SomeIndex d') ->
          if typeOf d' == typeRep (toDevRep d)
          then f (mkDevice Cthulhu (getIndex d))
          else error "register: type mismatch"
  where
    mv = getIndex d `Map.lookup` registry

registry :: IntMap (MVar SomeIndex)
registry = Map.fromList 
    $ map (second (unsafePerformIO . newMVar))
    $ [(1, SomeIndex DeviceARep) , (2, SomeIndex DeviceARep), (3, SomeIndex DeviceBRep)]
{-# NOINLINE registry #-}

doOnA :: DeviceA s -> SIO s ()
doOnA (DeviceA i) = SIO $ print i

test1 = withDevice (IA 1) $ \onDevice -> doOnA onDevice
-- Runtime exception:
test2 = withDevice (IA 3) $ \onDevice -> doOnA onDevice
-- Doesn't compile as 's' escapes from it's scope
-- test3 = withDevice (IB 2) $ return
