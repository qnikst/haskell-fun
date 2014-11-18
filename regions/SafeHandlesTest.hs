{-# LANGUAGE Rank2Types, TypeFamilies #-}

-- Handle-based IO with the assured open/close protocol, see README
-- This file contains the tests for SafeHandles.hs.
-- This is the final solution: lightweight monadic regions with
-- only type-level enforcement of region discipline

module SafeHandlesTest where

import SafeHandles			-- import the security kernel
import Control.Monad
import Control.Exception
import Control.Concurrent

-- single region test
test1 = runSIO (
    do
	 h1 <- newSHandle "fname1" ReadMode
	 h2 <- newSHandle "fname1" ReadMode
	 -- Can't allocate the handle outside the top region...
	 -- h3 <- liftSIO $ newSHandle "fname1" ReadMode
	 -- There is no region two levels up
         -- h3 <- liftSIO $ liftSIO $ newSHandle "fname1" ReadMode
         l1 <- shGetLine h1
	 return True
         -- Can't do that: r escapes
	 -- return h2
	)

-- multiple region test
test2 = runSIO (
    do
    h1 <- newSHandle "fname1" ReadMode
    h3 <- newRgn (
	 do
	 h2 <- newSHandle "fname2" ReadMode
	 h3 <- liftSIO (newSHandle "fname3" ReadMode)
	 -- Can't allocate the handle outside the top region...
	 -- h4 <- liftSIO $ liftSIO $ newSHandle "fname1" ReadMode
         l1 <- shGetLine h1
         l1 <- shGetLine h2
         l1 <- shGetLine h3
	 return h3 -- but this is OK: h3 assigned to the parent region
         -- Can't do that: r escapes
	 -- return h2
	)
    l1 <- shGetLine h1
    l1 <- shGetLine h3
    return l1)

test2' fname = do
  h1 <- newSHandle fname ReadMode
  -- If this line is uncommented, test2'r reports an error.
  -- Indeed, test2' must then be used within another region rather than
  -- at the `top level'. The reported error clearly states the 
  -- violation of the subtyping constraint: a child region computation
  -- cannot be coerced to the type of its ancestor
  -- h2 <- liftSIO $ newSHandle fname ReadMode
  return ()
test2'r = runSIO (test2' "fname")


	 
testmany = runSIO(
    do
    h1 <- newSHandle "fname1" ReadMode
    h5 <- newRgn (do
	    h2 <- newSHandle "fname2" ReadMode
	    newRgn (do
		    h3 <- newSHandle "fname3" ReadMode
		    newRgn (do
			    h4 <- newSHandle "fname4" ReadMode
			    l1 <- shGetLine h1
			    l2 <- shGetLine h2
			    l3 <- shGetLine h3
			    l4 <- shGetLine h4
			    h5 <- liftSIO $ liftSIO $ liftSIO 
			             (newSHandle "fname5" ReadMode)
			    return h5)))
    shGetLine h5)


			    

-- An attempt to leak the computation. 
-- Now, it won't work...
{- 
test2' = runSIO (
    do
    h1 <- newSHandle "fname1" ReadMode
    let c1 = shGetLine h1
    c1
    ac <- newRgn (
	 do
	 h2 <- newSHandle "fname2" ReadMode
	 -- Fake the SIO type. Won't work though: h2 handle contaminates...
	 return ((shGetLine h2) `asTypeOf` c1)
       )
    -- ac
    newRgn (do
	    -- That too is a type error: lack of polymorphism in newRgn
	    -- ac
	    return ()
	   )

    return True
   )
-}

{- 
-- The above error is merely due to force monomorphism in the
-- monadic bind (do ac <- ...). One may think that a higher-rank type 
-- may give us a way around the monomorphic binding in do, and 
-- so to defeat the safety.
-- Fortunately, our approach prevents such a `way-around' and so
-- safety is preserved.

newtype WC r1 = WC{unWC:: forall r2 . IORT r2 (IORT r1 IO) String}

test2'' = runSIO (
    do
    h1 <- newSHandle "/dev/null" ReadMode
    ac <- newRgn (
	 do
	 h2 <- newSHandle "/dev/null" ReadMode
	 -- Fake the IORT type. Won't work though... Good
	 return (WC (shGetLine h2))
       )
    -- unWC ac
    newRgn (do
	    -- If this were allowed, safety would have been defeated.
            -- Fortunately, we can't even construct the WC value:
            -- the type error is reported at `return (WC (shGetLine h2))'
            -- above.
	    unWC ac
	    return ()
	   )

    return True
   )

-}

-- Attempts to leak handles and computations via mutation
testref = runSIO (
    do
    h1 <- newSHandle "fname1" ReadMode
    rh <- sNewIORef undefined	-- a ref cell holding a handle
    let c1 = shGetLine h1
    c1
    ra <- sNewIORef undefined	-- a ref cell holding a computation
    newRgn (do
	 h2 <- newSHandle "fname2" ReadMode
	 -- sWriteIORef rh h1
	 -- sWriteIORef rh h2 -- type error, 's' of the inner region escapes
	 -- sWriteIORef ra (shGetLine h1) -- OK
	 -- sWriteIORef ra (liftSIO (shGetLine h2))
	 -- sWriteIORef ra (shGetLine h2) -- error: subtyping violation
	 return ()
       )
    newRgn (do
	    -- sReadIORef ra >>= id
	    return ()
	   )
    return True
   )


{- Ken's test:
A programming example using the enumerator (rather than cursor) pattern to
    (1) read a file name from a file
    (2) open that file and zip the two files' contents together
thus assuring that the files are accessed correctly and resources
disposed of completely.
-}


till condition iteration = loop where
  loop = do b <- condition
            if b then return () else iteration >> loop

test3 = runSIO (do
  h1 <- newSHandle "/tmp/SafeHandles.hs" ReadMode
  h3 <- newRgn (test3_internal h1)
  -- once we closed h2, we write the rest of h1 into h3
  till (shIsEOF h1)
       (shGetLine h1 >>= shPutStrLn h3)
  shReport "test3 done"
  )

-- The following shows that we do not have to put all IO code in
-- one big function. We can spread it out. The inferred type for the
-- following is _region-polymorphic_.
test3_internal h1 = do
  h2 <- newSHandle "/tmp/ex-file.conf" ReadMode
  fname <- shGetLine h2		-- read the fname from the config file
  -- allocate handle in the parent region
  h3 <- liftSIO (newSHandle fname WriteMode)
  -- zip h2 and h1 into h3
  shPutStrLn h3 fname
  till (liftM2 (||) (shIsEOF h2) (shIsEOF h1))
       (shGetLine h2 >>= shPutStrLn h3 >>
        shGetLine h1 >>= shPutStrLn h3)
  shReport "Finished zipping h1 and h2"
  return h3 -- but this is OK: h3 assigned to a parent region
  -- return h2 -- that would be an error: h2 can't escape



test4 h1 h2 = do
	      d1 <- shGetLine h1
	      shPutStrLn h2 d1

{-
Inferred type: region-polymorphic, as expected
test4
  :: (MonadRaise m1 m, MonadRaise m2 m, SMonadIO m) =>
     SHandle m1 -> SHandle m2 -> m ()
-}


-- Testing for problems in opening a file
-- We copy the contents of fname_in into fname_out.
-- If fname_in does not exist, write a message to fname_out to that effect.
-- Nothing bad happens if the file could not be opened as
-- no file reference (safe handle) is created in that case.

test_copy fname_in fname_out = do
  hout <- newSHandle fname_out WriteMode
  (do newRgn (do
        hin <- newSHandle fname_in ReadMode
        till (shIsEOF hin)
             (shGetLine hin >>= shPutStrLn hout))
      shReport "Finished copying")
   `shCatch` \e -> do
     shReport ("Exception caught: " ++ show (e::SomeException))
     shPutStrLn hout ("Copying failed: " ++ show e)

test_of1 = runSIO (test_copy "/etc/motd" "/tmp/t1")
test_of2 = runSIO (test_copy "/non-existent" "/tmp/t1")

-- Implement this test by Ken:
{-
It's actually not clear to me, in the solution you propose, what happens
when we have three regions (call them P, Q, R, from oldest to youngest)
and we first dup a handle from R to Q and then dup the same handle from
R to P.  Would the region library code know at run time whether to
forward all three copies of the same handle to Q or to P?
-}

-- Dynamically extending the lifetime of handles
test_dup = runSIO (do
  -- Region P
  hq <- newRgn (do			-- region Q
		hr <- newRgn(do 	-- region R
			     h2  <- newSHandle "/etc/motd" ReadMode
			     _   <- shDup h2 -- duplicates are OK
			     h2' <- shDup h2
			     return h2')
		shGetLine hr
		shReport "Region Q finished"
		shDup hr)
  shGetLine hq
  shReport "Outer region finished"
 )

-- Example suggested by Matthew Fluet

test5 = runSIO (do
  h <- newRgn (test5_internal "/tmp/ex-file2.conf")
  l <- shGetLine h
  shReport $ "Continuing processing the older file, read: " ++ l
  shReport "test5 done")

test5_internal conf_fname = do
  hc <- newSHandle conf_fname ReadMode
  fname1 <- shGetLine hc	-- read the fname from the config file
  fname2 <- shGetLine hc	-- read the other fname from the config file
  h1 <- newSHandle fname1 ReadMode
  h2 <- newSHandle fname2 ReadMode
  l1 <- shGetLine h1
  l2 <- shGetLine h2
  shReport $ "read entries: " ++ show (l1,l2)
  let (fname_old,h_old) | l1 < l2   = (fname2,h2)
                        | otherwise = (fname1,h1)
  shReport ("Older log file: " ++ fname_old)
  shDup h_old -- prolong the life of that handle



-- Issues with inferring region-polymorphic code
testp1 h = shGetLine h
-- testp1 :: (MonadRaise m1 m2, RMonadIO m2) => SHandle m1 -> m2 String

-- The following, essentially equivalent, code however gives problem
-- testp2 h = newRgn (shGetLine h)
-- Could not deduce (MonadRaise m1 (IORT s1 m)) from the context ()
-- And so does this
-- testp3 h = shGetLine h >> newRgn (shGetLine h)

-- But the following is OK:
-- We can provide the explicit signature
testp4 :: (SMonad1IO m2, m2 ~ (IORT s' m'), MonadRaise m1 m2) =>
	  SHandle m1 -> m2 String
testp4 h = newRgn (liftSIO $ shGetLine h)
-- The signature may be omitted. It will be inferred then:
{- inferred type is polymorphic as desired.
testp4 :: (RMonadIO m, MonadRaise m1 (IORT s m)) =>
          SHandle m1 -> IORT s m String
-}

-- usage example
testp4r = runSIO (do
  h1 <- newSHandle "/etc/motd" ReadMode
  testp4 h1)

test6f = runSIO (do
  h <- newSHandle "/etc/issue" ReadMode
  lock <- unsafelIO $ newEmptyMVar
  t <- regionForkIO $ unsafelIO $ putMVar lock ()
  unsafelIO $ takeMVar lock >> threadDelay 1000000
  l1 <- shGetLine h
  shReport $ "read entry: " ++ show l1)
         

test7f = runSIO (do
  h <- newSHandle "/etc/issue" ReadMode
  lock <- unsafelIO $ newEmptyMVar
  t <- regionForkIO $ do
    unsafelIO $ takeMVar lock >> threadDelay 1000000
    l1 <- shGetLine h
    shReport $ "read entry: " ++ show l1
  unsafelIO $ putMVar lock ())


test8f = runSIO (do
   h1 <- newSHandle "/etc/issue" ReadMode
   newRgn $ do
      h2 <- newSHandle "/etc/issue/" ReadMode
      t <- regionForkIO $ do
        l2 <- shGetLine h1
        l3 <- shGetLine h2
        return ()
      -- fork of parent region
      liftSIO $ newRgn $ do
         l4 <- shGetLine h1
         -- is not permited by typechecker
         -- l5 <- shGetLine h2
         return ()
      return ())

test9f = runSIO (do
   reg <- newRegionVar   
   regionForkIO $ do
     h1 <- getResource reg
     unsafelIO $ threadDelay 1000000
     l2 <- shGetLine h1
     shReport $ "read entry: " ++ show l2
   putResource reg =<< newSHandle "/etc/issue" ReadMode
   return ())

