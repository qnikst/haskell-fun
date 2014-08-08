> {-# LANGUAGE BangPatterns #-}

I want to play a bit with series expansion, for an example I'll
take a Taylor series.

For a function we can write a Taylor series expansion:

$$ f(x) = f(x_0) + \sum_{k=1}\frac{f^(k)}{k!}(x-x_0)^k $$

And I want to implement a small amount of code that can work
with such representations.

As a basic structure I'm going to use a stream of values `a`:

> data S a = S !a (S a)

Here we had a choice as both `List` and `Stream` can work for us.
More over `List` is more expressive as it's possible to write a
finite serie using `List`

If you'll use a streams for you then you can take a them from
[Stream](https://hackage.haskell.org/package/Stream) package. 
The implemetation is more or less the same as here.

Series is a Functor:

> instance Functor S where
>    fmap f (S a x) = S (f a) (fmap f x)

Also we may give a 'Num' instance for simplicity:

> instance Num a => Num (S a) where
>   (S a x) + (S b y) = S (a+b) (x+y)
>   abs s = fmap abs s
>   signum s = fmap signum s
>   (S a x) * (S b y) = S (a * b) (fmap (* a) y + fmap (*b) x + S 0 (x * y))
>   fromInteger x = S (fromInteger x) 0

Here are 2 tricky parts, first one is implementation of `*` second one
is implementation of `fromInteger`. For `*` we need to thing of an expansion
like of polymonial, i.e. `S a b = a + b * x`. Then we can write the following:

\begin{eqnarray}
 (S a x) * (S b y) = (a + x p) * (b + y p) = \\
  = (a b + a y p + b x p + x y p^2) = \\
  = S (a b) (a y + b x + x y p)     = \\
  = S (a b) (fmap (*a) y + fmap (*b) x + S 0 (x * y))
\end{eqnarray}

There are 2 possible implementations for fromInteger:
  
  1. `fromInteger x = S (fromInteger x) (fromInteger x)`

  2. `fromInteger x = S (fromInteger x) 0`

For `fromInteger` we need to select an implemnation such that `fromInteger 1 * a == a`
`fromInteger 0 + a = a` `fromInteger 0 * a == 0` for any `a`. So then we see that we
can select only second one, otherwise properties for `1` will not hold.


Now we can add a simple Fractional instance.

> instance Fractional a => Fractional (S a) where
>   fromRational x = S (fromRational x) (fromRational x)

In order to inspect a stream we can introduce a helper function:

> stake :: Int -> S a -> [a]
> stake 0 _       = []
> stake n (S a s) = a:stake (n-1) s

Here all functions will be prefixed with `s` however if you write a module for working
with streams you may prefer to not add it and ask user to import module qualified.

Here is a function that builds a stream from the list:

> fromList :: [a] -> S a
> fromList (x:xs) = S x (fromList xs) -- works only on infinite list
> 
> fromListNum :: Num a => [a] -> S a
> fromListNum [] = 0
> fromListNum (x:xs) = S x (fromListNum xs)


In order to write an usefull functions and series we will introduce a folding:

> sscan :: (a -> b -> a) -> a -> S b -> S a
> sscan f i (S x s) =
>  let k = f i x
>  in S k (sscan f k s)

Now lets introduce few functions using `sfold`. A function that will generate
a sum of the Stream, i.e. having a stream `ssum <a0:a1:a2:..> = <a0:a0+a1:a0+a1+a2:...>`

> ssum :: Num a => S a -> S a
> ssum = sscan (+) 0

Build a stream by iterating a function

> siterate :: (a -> a) -> a -> S a
> siterate f x = S x (siterate f (f x))

Build a serie of powers: $<x,x^2,x^3,...>$

> spower x = siterate (*x) x

Unfold

> sunfold :: (c -> (c,a)) -> c -> S a
> sunfold f k = let (k',a) = f k in S a (sunfold f k')

Because we want to implement a teylor serie we want to have a serie of factorials

> sfac :: (Enum a, Num a) => S a
> sfac = sscan (*) 1 (fromList [1..]) 

And now 1/factorials

> sdfac :: Fractional a => S a
> sdfac = fmap (\x -> 1 / (fromIntegral x)) sfac

> szipWith :: (a -> b -> c) -> S a -> S b -> S c
> szipWith f (S a x) (S b y) = S (f a b) (szipWith f x y)

Now we can write an element-vise  multiplication.

> (^*) :: Num a => S a -> S a -> S a
> (^*) = szipWith (*)

This is an actual building of the Taylor serie:

> build :: Fractional a => a -> S a -> S a
> build t s = ssum $ spower t ^* s


> shead :: S a -> a
> shead (S a s) = a

> stail :: S a -> S a
> stail (S a s) = s

> sdropWhile :: (a -> Bool) -> S a -> S a
> sdropWhile p s@(S a xs)
>   | p a = sdropWhile p xs
>   | otherwise = s

> eps :: (Ord a, Num a) => a -> S a -> a
> eps e s = snd . shead . sdropWhile (\(x,y) -> abs (x - y) < e)
>         $ szipWith (,) s (stail s)

> texp :: Fractional a => S a
> texp = S 1 sdfac

> test1 = eps 0.05 $ build 1 texp

> test2 = eps 0.05 $ build 1 $ texp + texp

> test3 = eps 0.05 $ build 1 $ texp * (S 2 (S 2 0))


