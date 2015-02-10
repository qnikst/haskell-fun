{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}

import           Data.Bifunctor (second)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import           Control.Concurrent
import           System.IO.Unsafe
import           Data.Typeable

type Index = Int

data DeviceType
        = TA
        | TB

deriving instance Typeable 'TA
deriving instance Typeable 'TB

data Device :: DeviceType -> * where
    A :: Index -> Device TA
    B :: Index -> Device TB

deriving instance Typeable Device

deviceIndex :: Device a -> Index
deviceIndex (A i) = i
deviceIndex (B i) = i

doOnA :: Wtf s a -> SIO s ()
doOnA (Wtf i) = SIO $ putStrLn $ "A action on " ++ show i

doOnB :: Wtf s b -> SIO s ()
doOnB (Wtf i) = SIO $ putStrLn $ "B action on " ++ show i

data SomeDevice = forall a . Typeable a => SomeDevice (Device a)

newtype Wtf s (a :: DeviceType) = Wtf Int

newtype SIO s a = SIO {runSIO :: IO a}


withDevice :: Typeable a => Device a -> (forall s . Wtf s a -> SIO s b) -> IO b
withDevice d f = registering d $ \i -> runSIO $ f (Wtf i)


registering :: Typeable a => Device a -> (Int -> IO b) -> IO b
registering d f =
    case mv of
      Nothing -> error "register: no such device"
      Just mtx  -> 
        withMVar mtx $ \(SomeDevice d') ->
          if typeRep d' == typeRep d
          then f (deviceIndex d)
          else error "register: type mismatch"
  where
    mv = deviceIndex d `Map.lookup` registry

registry :: IntMap (MVar SomeDevice)
registry = Map.fromList 
    $ map (second (unsafePerformIO . newMVar))
    $ [(1,SomeDevice (A 1)), (2, SomeDevice (A 2)), (3, SomeDevice (B 3))]
{-# NOINLINE registry #-}

test1 = withDevice (A 1) $ \onDevice -> doOnA onDevice
test2 = withDevice (B 2) $ \onDevice -> doOnA onDevice
-- Escape:
-- test3 = withDevice (B 2) $ return
