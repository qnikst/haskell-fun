-- by Oleg  Kiselyov
-- URL: http://okmij.org/ftp/Computation/resource-aware-prog/SafeHandles.hs
{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE UndecidableInstances, OverlappingInstances #-}

-- Handle-based IO with the assured open/close protocol, see README
-- This file contains the Security kernel. See SafeHandlesTest.hs for tests.
-- This is the final solution: lightweight monadic regions with
-- only type-level enforcement of region discipline

module SafeHandles
    (IORT,			-- constructors not exported
     SIO,
     SHandle,
     SMonad1IO,				-- for the sake of signatures
     SMonadIO,				-- for the sake of signatures
     MonadRaise(..),

     runSIO,
     newRgn,
     liftSIO,
     newSHandle,
     shDup,
     IOMode(..),		-- re-exported from System.IO
     BufferMode(..),		-- re-exported from System.IO

     shGetLine,
     shPutStrLn,
     shPutStr,
     shIsEOF,
     shSetBuffering,

     -- ByteString
     shGetSome,
     shPut,

     shThrow,
     shCatch,
     shReport,

     sNewIORef,			-- IORef in SIO
     sReadIORef,
     sWriteIORef,

     -- Concurrency
     regionForkIO,
     newRegionVar,
     getResource,
     putResource,
     unsafelIO
     ) where

import System.IO
import Control.Applicative
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans
import Data.IORef
import Data.List (find)
import Prelude hiding (catch)

import qualified Data.ByteString as B		-- for byte-string operations

import Control.Concurrent
import Control.Monad.Identity

-- The IO monad with safe handles and regions (SIO) is implemented as 
-- the monad transformer IORT (recursively) applied to IO.
--
-- Each region maintains the state listing all open 
-- handles assigned to the region.
-- Since we already have IO, it is easy to implement the state as a 
-- mutable list (IORef of the list) and make this reference
-- a pervasive environment. 
-- We could have used implicit parameters or implicit configurations to
-- pass that IORef around. Here, we use ReaderT.

-- Since we do IO with our handles, we may be tempted to make
-- the IORT transformer an instance of MonadIO. However, that would 
-- give the user the ability to do any IO and so defeat the safety
-- guarantees. The purpose of IORT is to restrict permissible IO
-- operations to an assured set. Since we do need something like MonadIO,
-- we define our own private version here, RMonadIO. It is not exported.
-- Unlike MonadIO, our RMonadIO also supports catching and 
-- handling of exceptions.

-- A region is identified by a label, a type eigenvariable (see 's'
-- in ST s).

newtype IORT s m v = IORT{ unIORT:: ReaderT (IORef [HandleR]) m v } 
    deriving (Functor, Applicative, Monad)

type SIO s = IORT s IO

-- A simple abstract data type to track the duplication of handles
-- using reference counting
data HandleR = HandleR Handle (IORef Integer)

close_hr :: HandleR -> IO ()
close_hr (HandleR h refcount) = do
    hPutStrLn stderr $ "Closing " ++ show h
    rc <- readIORef refcount
    assert (rc > 0) (return ())
    writeIORef refcount (pred rc)
    if rc > 1
       then hPutStrLn stderr "   Aliased handle wasn't closed"
       else hClose h

new_hr :: Handle -> IO HandleR
new_hr h = newIORef 1 >>= return . HandleR h

eq_hr :: Handle -> HandleR -> Bool
eq_hr h1 (HandleR h2 _) = h1 == h2



-- Lift from one IORT to an IORT in a children region...
-- IORT should be opaque to the user: hence this is not the instance 
-- of MonadTrans
liftSIO :: Monad m => IORT s m a -> IORT s1 (IORT s m) a
liftSIO = IORT . lift

-- Raise from one IORT to another: this class lets the user access
-- handles of an ancestor region
-- MonadRaise m1 m2 holds when either 
--   - m2 is the same as m1, or
--   - m2 is the sequence of one or more IORT applied to m1.
-- In other words, MonadRaise m1 m2 holds if m1 is an improper
-- `suffix' of m2.

class (Monad m1, Monad m2) => MonadRaise m1 m2 where
    lifts :: m1 a -> m2 a


-- The following is the general definition. It required the following extensions
-- {-# LANGUAGE EmptyDataDecls #-}
-- {-# LANGUAGE UndecidableInstances, OverlappingInstances #-}
-- to implement the disjunction present in the definition of MonadRaise.
-- In particular, OverlappingInstances are needed for TEQ.

-- We can actually eliminate overlapping if we write a type function
-- to reverse the order of IORT layers. We would search for a prefix
-- then, which is much easier.

{-
 -- more complicated implementation
instance (TEQ m1 m2 eq, MonadRaise' m1 m2 eq) => MonadRaise m1 m2

class (Monad m1, Monad m2) => MonadRaise' m1 m2 eq
instance (Monad m1, Monad m2) => MonadRaise' m1 m2 HTrue
instance MonadRaise m1 m2 => MonadRaise' m1 (IORT s m2) HFalse

data HTrue
data HFalse

class TEQ (a :: * -> *) (b :: * -> *) eq | a b -> eq
instance TEQ a a HTrue
instance TypeCast eq HFalse => TEQ a b eq

-}

-- {-
-- A simpler implementation
instance Monad m => MonadRaise m m where
    lifts = id

instance (Monad m2, m2 ~ (IORT s m2'), MonadRaise m1 m2') 
    => MonadRaise m1 m2 where
    lifts = IORT . lift . lifts

-- -}



-- In GHC 6.6 and earlier, we could only write the following specialized
-- definition. It handles only the fixed number of levels.
{-
instance Monad m => MonadRaise m m
instance Monad m => MonadRaise m (IORT s1 m)
instance Monad m => MonadRaise m (IORT s2 (IORT s1 m))
instance Monad m => MonadRaise m (IORT s3 (IORT s2 (IORT s1 m)))
-}

-- RMonadIO is an internal class, a version of MonadIO
class Monad m => RMonadIO m where
    brace :: m a -> (a -> m b) -> (a -> m c) -> m c
    snag  :: Exception e => m a -> (e -> m a) -> m a
    lIO   :: IO a -> m a

instance RMonadIO IO where
    brace = bracket
    snag  = catch
    lIO   = id

-- The following makes sure that a low-level handle (System.IO.Handle)
-- cannot escape in an IO exception. Whenever an IO exception is caught,
-- we remove the handle from the exception before passing it to the
-- exception handler.
-- With GHC 7.x, this sanitization is no longer needed: IOException is
-- opaque (the data constructors of IOError are not exported from
-- GHC.IO.Exceptions). So, the user cannot get hold of a handle
{-
catch':: Exception e => IO a -> (e -> IO a) -> IO a
catch' m f = catch m (f . sanitizeExc)

bracket' :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket' before after m = 
    bracket before after m `catch` (throwIO . sanitizeExc)

sanitizeExc :: IOException -> IOException
sanitizeExc e = 
    maybe e (\e -> IOException e{ioe_handle = Nothing}) $ ioErrors e
-}

instance RMonadIO m => RMonadIO (ReaderT r m) where
    brace before after during = ReaderT (\r ->
	let rr :: RMonadIO m => ReaderT r m a -> m a
            rr m = runReaderT m r
	in brace (rr before) (rr.after) (rr.during))
    snag m f = ReaderT(\r -> 
		 runReaderT m r `snag` \e -> runReaderT (f e) r)
    lIO = lift . lIO

instance RMonadIO m => RMonadIO (IORT s m) where
    brace before after during = IORT
	(brace (unIORT before) (unIORT.after) (unIORT.during))
    snag m f = IORT ( unIORT m `snag` (unIORT . f) )
    lIO = IORT . lIO

-- A safe version of RMonadIO, which is exported
-- SMonadIO is useful in writing signatures
class RMonadIO m => SMonadIO m
instance RMonadIO m => SMonadIO (IORT s m)

-- A different version that enforces the fact the monad must be
-- of the form (IORT s m). It also _provides_ (rather than requires) that 
-- m is a member of RMonadIO.
class RMonadIO (UnIORT m) => SMonad1IO m
instance RMonadIO m => SMonad1IO (IORT s m)

type family UnIORT (m :: * -> *) :: * -> *
type instance UnIORT (IORT s m) = m

-- There is no explicit close operation. A handle is automatically
-- closed when its region is finished (normally or abnormally).

newRgn :: RMonadIO m => (forall s. IORT s m v) -> m v
newRgn m = brace (lIO (newIORef [])) after (runReaderT (unIORT m))
    where after handles = lIO (readIORef handles >>= mapM_ close)
          close h = catch (close_hr h) (\(e::SomeException) -> return ())

runSIO :: (forall s. SIO s v) -> IO v
runSIO = newRgn

-- Our (safe) handle is labeled with the monad where it was created
newtype SHandle (m :: * -> *) = SHandle Handle	-- data ctor not exported

-- Create a new handle and assign it to the current region 
-- One can use liftIORT (newSHandle ...) to assign the handle to any parent
-- region.
newSHandle :: (m ~ (IORT s' m'), SMonad1IO m) => 
	      FilePath -> IOMode -> m (SHandle m)
newSHandle fname fmode = IORT r'
 where r' = do
	    h <- lIO $ openFile fname fmode -- may raise exc
	    handles <- ask
	    hr <- lIO $ new_hr h
	    lIO $ modifyIORef handles (hr:)
	    return (SHandle h)


-- Safe-handle-based IO...
-- The handle is assigned to the current region or its ancestor.
-- So, we have to verify that the label of the handle is the prefix
-- (perhaps improper) of the label of the monad (label of the region).

shGetLine :: (MonadRaise m1 m2, SMonadIO m2) => SHandle m1 -> m2 String
shGetLine (SHandle h) = lIO (hGetLine h)

shPutStrLn :: (MonadRaise m1 m2, SMonadIO m2) => SHandle m1 -> String -> m2 ()
shPutStrLn (SHandle h) = lIO . hPutStrLn h

shPutStr :: (MonadRaise m1 m2, SMonadIO m2) => SHandle m1 -> String -> m2 ()
shPutStr (SHandle h) = lIO . hPutStr h

shIsEOF :: (MonadRaise m1 m2, SMonadIO m2) => SHandle m1 -> m2 Bool
shIsEOF (SHandle h) = lIO (hIsEOF h)

shSetBuffering :: (MonadRaise m1 m2, SMonadIO m2) => 
		  SHandle m1 -> BufferMode -> m2 ()
shSetBuffering (SHandle h) = lIO . hSetBuffering h

shGetSome :: (MonadRaise m1 m2, SMonadIO m2) =>
	     SHandle m1 -> Int -> m2 B.ByteString
shGetSome (SHandle h) = lIO . B.hGetSome h

shPut :: (MonadRaise m1 m2, SMonadIO m2) => SHandle m1 -> B.ByteString -> m2 ()
shPut (SHandle h) = lIO . B.hPut h


-- Duplicate a handle, returning a handle that can be used in the parent
-- region (and can be returned from the current region as the result).
-- This operation prolongs the life of a handle based on a
-- _dynamic_ condition. If we know the lifetime of a handle statically,
-- we can execute liftSIO (newSHandle ...) to place the handle in the
-- corresponding region. If we don't know the lifetime of a handle
-- statically, we place it in the inner region, and then extend its lifetime
-- by reassigning to the parent region based on the dynamic conditions.
shDup :: (m ~ (IORT s' m'), SMonad1IO m) => 
	 SHandle (IORT s1 m) -> IORT s1 m (SHandle m)
shDup (SHandle h) = IORT (do
   handles <- ask >>= lIO . readIORef
   let Just hr@(HandleR _ refcount) = find (eq_hr h) handles
   lIO $ modifyIORef refcount succ
   lift $ IORT (do		-- in the parent monad
		  handles <- ask
		  lIO $ modifyIORef handles (hr:))
   return (SHandle h))

-- It seems however that IOErrors don't invalidate the Handles.
-- For example, if EOF is reported, we may try to reposition the `file'
-- and read again. That's why in Posix, EOF and file errors can be cleared.

shThrow :: Exception e => SMonadIO m => e -> m a
shThrow = lIO . throwIO

shCatch :: Exception e => SMonadIO m => m a -> (e -> m a) -> m a
shCatch = snag

-- Useful for debugging
shReport :: SMonadIO m => String -> m ()
shReport = lIO . hPutStrLn stderr

-- make IORef available with SIO, so we may write tests that attempt
-- to leak handles and computations with handles via assignment

sNewIORef :: SMonadIO m => a -> m (IORef a)
sNewIORef = lIO . newIORef

sReadIORef :: SMonadIO m => IORef a -> m a
sReadIORef = lIO . readIORef

sWriteIORef :: SMonadIO m => IORef a -> a -> m ()
sWriteIORef r v = lIO $ writeIORef r v


{-
-- Standard HList stuff... No longer needed with GHC 7.x

class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x


class TypeCast2   (a :: * -> *) (b :: * -> *)   | a -> b, b->a
class TypeCast2'  t (a :: * -> *) (b :: * -> *) | t a -> b, t b -> a
class TypeCast2'' t (a :: * -> *) (b :: * -> *) | t a -> b, t b -> a
instance TypeCast2'  () a b => TypeCast2 a b
instance TypeCast2'' t a b => TypeCast2' t a b
instance TypeCast2'' () a a

-}


-- Concurrency
-- First attempt to duplicate a region externally
regionForkIO :: (RMonadIO m, Retransmit IO m) => IORT s m () -> IORT s m ThreadId
regionForkIO m = do
  runHier <- cloneHier
  IORT $ do
    lIO $ forkIO $ runHier m

-- In order to fork of a thread we should be able to recreate the same
-- enviromnent in a separate thread. Here we present a simple way of
-- imprementing this functionality.
--
-- This is not a safe approach unless it's not hiden from user.
--
-- TODO: implement in exception safe way, cuurently if asynchronous
--       exception arrives in the middle of execution then we may have problems
--       as reference count of some handles was already incremented
cloneHier :: (Retransmit IO (IORT s p)) => IORT s p (IORT s p a -> IO a) 
cloneHier = cln

class Monad n => Retransmit n m where
  cln :: m (m a -> n a)

instance Retransmit IO IO where cln = return id
instance (RMonadIO n, Retransmit IO n) => Retransmit IO (IORT s n) where
  cln = IORT $ ReaderT $ \env -> do
           g <- cln
           return $ \f -> g $ brace (before env >> return env) (after) (runReaderT (unIORT f))
   where
    before handles = lIO $ readIORef handles >>= mapM_ (\(HandleR _ t) -> modifyIORef' t succ)
    after handles = lIO $ readIORef handles >>= mapM_ close
    close h = catch (close_hr h) (\(e::SomeException) -> return ()) -- also catches asynchonous exceptions

-- Passing values between different regions
class RMonadIO n => Duplicate n m where dups :: SHandle m -> m (SHandle n)
instance RMonadIO n => Duplicate (IORT s n) (IORT s n) where
   dups x@(SHandle h) = IORT (do
     handles <- ask >>= lIO . readIORef
     let Just hr@(HandleR _ refcount) = find (eq_hr h) handles
     lIO $ modifyIORef refcount succ
     return x)
instance RMonadIO n => Duplicate (IORT s n) (IORT s (IORT s n)) where dups = shDup
instance (RMonadIO m, RMonadIO n, Duplicate n (IORT s m)) => Duplicate n (IORT s (IORT s m)) where
  dups (SHandle r)  = liftSIO $ dups (SHandle r)

data RegionVar s = RV (MVar (SHandle s))

newRegionVar :: RMonadIO s => IORT p s (RegionVar (IORT p s))
newRegionVar = lIO $ fmap RV newEmptyMVar

putResource :: (RMonadIO s2, Duplicate s1 (IORT p s2)) => RegionVar s1 -> SHandle (IORT p s2) -> IORT p s2 ()
putResource (RV reg) h = do h' <- dups h
                            lIO $ putMVar reg h'

getResource :: (RMonadIO s2) => RegionVar s1 -> IORT p s2 (SHandle s1)
getResource (RV reg) = lIO $ takeMVar reg 

-- For debug reasons:
unsafelIO :: RMonadIO m => IO a -> m a
unsafelIO = lIO
