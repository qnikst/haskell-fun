
> import Data.Stream (Stream(..), (<:>))
> import qualified Data.Stream as S

> newtype T a = T { unT :: Stream a }

> instance Functor T where
>    fmap f (T s) = T (fmap f s)

Also we may give a 'Num' instance for simplicity:


> instance Num a => Num (T a) where
>   (T x) + (T y) = T (S.zipWith (+) x y)
>   abs s = fmap abs s
>   negate s = fmap negate s
>   signum s = fmap signum s
>   (T (Cons a x)) * (T (Cons b y)) = T $ (a * b) <:> unT ( T (fmap (*a) y) + T (fmap (*b) x) + T (0 <:> unT (T x * T y)))
>   fromInteger x = T $ (fromInteger x) <:> unT 0
  
> instance Fractional a => Fractional (T a) where
>   fromRational x = T (fromRational x <:> unT 0)

> instance Floating a => Floating (T a) where
>   pi = T $ pi <:> unT 0

> power :: Num a => a -> T a
> power x = T $ 1 <:> S.iterate (*x) x

> (^.^) :: Num a => T a -> T a -> T a
> s1 ^.^ s2 = go s1 (power s2)
>    where
>     go :: Num a => T a -> T (T a) -> T a
>     go a (T (Cons b bs)) = T $ S.zipWith (+) (unT (a * b)) (0 <:> unT (go a (T bs)))


