{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Vinyl
import Data.Proxy

-- Lookup value in Record
class ZLookup s a u where
    zlookup :: proxy a -> Rec s u -> s a

instance ZLookup s a (a ': as) where
    zlookup _ (v :& _) = v

instance ZLookup s a bs => ZLookup s a (b ': bs) where
    zlookup f (_ :& r) = zlookup f r

-- | Apply function and read next element
zapply' :: ZLookup s a u => (s a -> b) -> Rec s u -> b
zapply' f r = f (zlookup (proxyF f) r)


type family Que k f where
    Que '[] a = a
    Que (k ': ks) (a -> b) = b

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
