{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Data.Vinyl
import Data.Vinyl.TypeLevel
import Data.Proxy
import Data.Type.Equality
import Data.Functor.Identity

type family Head u where Head (u ': us) = '[u]

type family Tail u where Tail (u ': us) = us
{-
-- Lookup value in Record
class ZLookup s a u where
    zlookup :: proxy a -> Rec s u -> s a

instance ZLookupI (a == b) s a (b ': bs) => ZLookup s a (b ': bs) where
    zlookup p r = zlookupi (Proxy :: Proxy (a == b)) p r

class ZLookupI (t::Bool) (s :: * -> *) (a :: *) (u :: [*]) where
    zlookupi :: proxy1 t -> proxy2 a -> Rec s u -> s a

instance ZLookupI ('True) s a (a ': as) where
    zlookupi _ _ (v :& _) = v

instance ZLookup s a bs => ZLookupI ('False) s a (b ': bs) where
    zlookupi _ p (_ :& r) = zlookup p r
-}

class GetLast u r | u -> r where
    getLast :: Rec s u -> s r

instance GetLast '[u] u where
    getLast (a :& _) = a

instance GetLast (u2 ': us) r => GetLast (u ': u2 ': us) r where
    getLast (_ :& as) = getLast as

class GetInit u where
    getInit :: Rec s u -> Rec s (Init u)

instance GetInit '[u] where
    getInit _ = RNil

instance GetInit (u2 ': us) => GetInit (u1 ': u2 ': us) where
    getInit (a :& as)  = a :& getInit as

class GetInit' u r where
    getInit' :: Rec s u -> Rec s r

instance GetInit' '[u] '[] where
    getInit' _ = RNil

instance GetInit' (u2 ': us) r => GetInit' (u1 ': u2 ': us) (u1 ': r) where
    getInit' (a :& as)  = a :& getInit' as

-- type Rot u = Last u ': Init u
--type Unrot u = Tail u ++ Head u 



class Append s u p | s u -> p where
    append :: Rec r s -> Rec r u -> Rec r p

instance Append '[] u u where
    append _ v = v

instance Append ss u p => Append (s ': ss) u (s ': p) where
    append (s :& ss) v = s :& append ss v

test = getLast (Identity (10::Int) :& RNil)

type family Last (u :: [*]) where
    Last (Apnd a b) = Last b
    Last '[u] = '[u]
    Last (u ': us) = Last us

rlast :: Rec s u -> Rec s (Last u)
rlast (a :& RNil) = a :& RNil
rlast (a :& as@( _ :& _)) = rlast as

rlast' :: GetLast (xs ++ '[x]) x => Rec s (xs ++ '[x]) -> s x
rlast' r = getLast r

type family Init (u :: [*]) where
    Init '[u] = '[]
    Init (u ': us) =  u ': Init us

rinit :: Rec s u -> Rec s (Init u)
rinit (a :& RNil) = RNil
rinit (a :& as@(_ :& _)) = a :& rinit as

rhead :: Rec s u -> Rec s (Head u)
rhead (a :& _) = a :& RNil

rtail :: Rec s u -> Rec s (Tail u)
rtail (_ :& as) = as

{-
rotate :: Rec s u -> Rec s (Rot u)
rotate r = rlast r :& rinit r
-}

{-
unrotate :: Rec s u -> Rec s (Unrot u)
unrotate r = rtail r <+> rhead r

rid :: (Unrot (Rot u) ~ u) => Rec s u -> Rec s u
rid = unrotate . rotate
-}

type family Apnd u s where
    Apnd '[] s = s
    Apnd (u ': us) s = u ': Apnd us s

rapp :: Rec s a -> Rec s b -> Rec s (Apnd a b)
rapp RNil b = b
rapp (a :& as) b = a :& rapp as b

rotate2   r = getLast r :& getInit r
unrotate2 r = getInit r <+> getLast r :& RNil

rotate3   r = getLast r :& getInit' r

class Rotate u where
    type RotR u :: [*]
    rot :: Rec s u -> Rec s (RotR u)
    unrot :: Rec s (RotR u) -> Rec s u

instance Rotate '[u] where
    type RotR '[u] = '[u]
    rot = id
    unrot = id

instance Rotate (u1 ': u2 ': us)  where
    type RotR (u1 ': u2 ': us) = Apnd (u2 ': us) '[u1]
    rot r = rtail r `rapp` rhead r
    unrot r = rlast r `rapp` rinit r

{-
rid' :: Rec s u -> Rec s u
rid' = rot . unrot
-}

{-
instance Rotate '[] '[] where
    rot RNil = RNil

instance Rotate '[u] '[u] where
    rot (a :& RNil) = a :& RNil

instance Rotate (u1 ': u2 ': us) (Last us ': u1 ': u2) where
    rot = undefined
    -}


-- test2 :: getLast $ (Identity (10::Int) :& RNil) 
{-
instance ZLookup s a (a ': as) where
    zlookup _ (v :& _) = v

instance ZLookup s a bs => ZLookup s a (b ': bs) where
    zlookup f (_ :& r) = zlookup f r
-}

{-
-- | Apply function and read next element
zapply' :: ZLookup s a u => (s a -> b) -> Rec s u -> b
zapply' f r = f (zlookup (proxyF f) r)

class ZApplyC (u'::[*]) s f z u where
    zapplyC :: proxy u' -> f -> Rec s u -> z

instance ZApplyC '[] s f f u where
    zapplyC _ f _ = f

instance (ZApplyC xs s b z u, ZLookup s a u) => ZApplyC (x ': xs) s (s a -> b) z u where
    zapplyC p f r = zapplyC (tailP p) (zapply' f r) r

tailP :: proxy (u ': us) -> Proxy us
tailP _ = Proxy

proxyF :: (s a -> b) -> Proxy a
proxyF _ = Proxy

zapply :: ZApplyC u s f z u => f -> Rec s u -> z
zapply f r = zapplyC r f r

newtype Const a = Const a

test = zapply f $ (Const (4::Int)) :& (Const True) :& (Const '7') :& RNil
    where f :: Const Char -> Const Int -> Const Bool -> String
          f (Const a) (Const b) (Const c) = show a ++ show b ++ show c

-}
