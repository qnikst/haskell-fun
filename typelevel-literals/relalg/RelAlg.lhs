> {-# LANGUAGE TypeOperators #-}

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE GADTs #-}


> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE AllowAmbiguousTypes #-}

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE FunctionalDependencies #-}

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UnicodeSyntax #-}

Пока все умные люди занимаются обсуждением records и OFR мы попробуем в очередной
раз изобрести колесо и попытаться сделать гетерогенные записи.
Поскольку просто делать такие записи не интересно, то попытаемся так же
представить простенькую библиотеку для реляционной алгебры.


> import GHC.TypeLits
> import Data.Type.Equality
> import Data.Proxy

> import           Data.Set (Set)
> import qualified Data.Set as Set
> import           Data.Monoid
> import           Unsafe.Coerce

Для дальшейней работы нам потребуется два типа, тут можно обойтись и одним, но
так проще. Для начала введем синоним типа обозначающий, что поле "вида" @а@, имеет
тип @b@. 

> data (a::Symbol) :-> b = V b

Так же введем тип отвечающий предыдущему на уровне типов

> data a :--> b 


Теперь все готово для ввода гетерогенных записей, т.е. записей вида ключ-значение.

> data HRec :: [*] -> * where
>   HNil  :: HRec '[]
>   HCons :: a -> HRec xs -> HRec ((b :-> a) ': xs)

Давайте рассмотрим эту запись, перед нами предстало обобщенный алгебраический тип
параметризованный списком на уровне типов, этот список показывает какие связки
тип значение у нас имеются.

> instance Show (HRec '[]) where show _ = "HNil"

> instance (KnownSymbol s, Show (HRec xs), Show a) => Show (HRec ((s :-> a) ': xs)) where
>    show as@(HCons _ hs)  = "HCons (" ++ inner Proxy as ++ ") (" ++ show hs ++ ")"
>      where
>        inner :: (Show a, KnownSymbol s) => Proxy s -> HRec ((s :-> a)':xs) -> String
>        inner p (HCons a _) = symbolVal p ++ " :-> " ++ show a

*Main> let t = (HCons "q" (HCons 5 HNil) :: HRec ["Name" :-> String, "Age" :-> Int])
*Main> :t t
t :: HRec '["Name" :-> String, "Age" :-> Int]
*Main> show t
"HCons (Name :-> \"q\") (HCons (Age :-> 5) (HNil))"

> instance Eq (HRec '[]) where _ == _ = True
> instance (Eq b, Eq (HRec xs)) => Eq (HRec ((a :-> b) ': xs)) where
>   (HCons a as) == (HCons b bs) = a == b && as == bs

> instance Ord (HRec '[]) where compare _ _ = EQ

> instance (Ord b, Ord (HRec xs)) => Ord (HRec ((a :-> b) ': xs)) where
>    (HCons a as) `compare` (HCons b bs) = a `compare` b <> as `compare` bs


Для множеств используем обертку типа:

> newtype RelSet xs = RS { unRS :: Set (HRec xs) }

Тут вполне можно использовать и синоним типа, но это будет хуже, т.к. newtype является
ещё и прокси и может служить для передачи информации о типе.


Начнем с простых операций над множествами:

\### Объедиение

> union :: Ord (HRec xs) => RelSet xs -> RelSet xs -> RelSet xs
> union (RS sx) (RS sy) = RS $ sx `Set.union` sy

\### Пересечение

> intersection :: Ord (HRec xs) => RelSet xs -> RelSet xs -> RelSet xs
> intersection (RS sx) (RS sy) = RS $ sx `Set.intersection` sy

\### Вычитание

> subtraction :: Ord (HRec xs) => RelSet xs -> RelSet xs -> RelSet xs
> subtraction (RS sx) (RS sy) = RS $ sx Set.\\ sy


Данные операции пишутся банально и не интересны для нас. Так что можно переходить
к более сложным.

\### Переименование

Переименованием называется унарная операция $\rho_{a / b}(R)$ результатом которой является
множество равное $R$, за исключением того, что все аттрибуты @a@ в кортежах переименованы в @b@. 

Рассмотрим сначала переименование на типах, т.е. если у нас есть список [s -> a], то мы хотим
переименовать часть s в g. Это можно сделать рекурсивно, для каждого из элементов списка G, мы
проходим по всем элементам списка a, переименовывая если имена совпадают. Плюс используем тот
факт, что данные не повторяются.

> type family Rename (a :: [*]) (b :: [*]) :: [*] where
>        Rename '[]       bs = '[]
>        Rename (a ': as) bs = RenameInner a bs ': Rename as bs

> type family RenameInner (a :: *) (b :: [*]) :: * where
>        RenameInner     a         '[]          = a
>        RenameInner (s :-> a) (s :--> g ': bs) = g :-> a
>        RenameInner     a     (    g    ': bs) = RenameInner a bs


Примеры использования:

*Main> :kind! Rename ["Foo" :-> Int,"Bar" :-> ()] ["Foo" :--> "Bar", "Bar" :--> "Foo"]
Rename ["Foo" :-> Int,"Bar" :-> ()] ["Foo" :--> "Bar", "Bar" :--> "Foo"] :: [*]
= '["Bar" :-> Int, "Foo" :-> ()]

Так же мы можем использовать это при определении записей:

*Main> show (HCons 5 (HCons () HNil) :: HRec (Rename ["Foo" :-> Int,"Bar" :-> ()] ["Foo" :--> "Bar", "Bar" :--> "Foo"]))
"HCons (Bar :-> 5) (HCons (Foo :-> ()) (HNil))"

*Main> let t = HCons 5 (HCons () HNil) :: HRec ["Foo" :-> Int,"Bar" :-> ()]
*Main> it
t :: HRec '["Foo" :-> Int, "Bar" :-> ()]
*Main> t :: HRec ["Bar" :-> Int, "Foo" :-> ()]
<interactive>:35:1:
    Couldn't match type ‘"Foo"’ with ‘"Bar"’
    Expected type: HRec '["Bar" :-> Int, "Foo" :-> ()]
      Actual type: HRec '["Foo" :-> Int, "Bar" :-> ()]
    In the expression: t :: HRec '["Bar" :-> Int, "Foo" :-> ()]
    In an equation for ‘it’:
        it = t :: HRec '["Bar" :-> Int, "Foo" :-> ()]

> rrename :: Rename' HRec a (Rename a b) => HRec a -> proxy b -> HRec (Rename a b)
> rrename h r = rrename' h (pp h r)
>  where pp :: proxy1 a -> proxy2 b -> Proxy (Rename a b)
>        pp _ _ = Proxy
   
> class Rename' s a b where
>   rrename' :: s a -> proxy b -> s b

> instance Rename' HRec '[] '[] where
>   rrename' HNil _ = HNil

> instance Rename' HRec as bs => Rename' HRec (x :-> a ': as) (y :-> a ': bs) where
>   rrename' (HCons a as) p = HCons a (rrename' as (pp p))
>      where pp :: proxy (a ': as) -> Proxy as
>            pp _ = Proxy

Пример:

*Main> rrename t (Proxy :: Proxy '["Foo" :--> "Zoo"])
HCons (Zoo :-> 5) (HCons (Bar :-> ()) (HNil))


Но как внимательный читатель уже заметил преставление соверешенно не изменяется,
поэтому справедливо будет следующая реализация:

> rename'' :: HRec a -> proxy b -> HRec (Rename a b)
> rename''  h _ = unsafeCoerce h

*Main> rrename'' t (Proxy :: Proxy '["Foo" :--> "Zoo"])
HCons (Zoo :-> 5) (HCons (Bar :-> ()) (HNil))

Задание, проверим, что будет если мы попробуем переименовать несуществующее поле

*Main> :kind! Rename ["Foo" :-> Int,"Bar" :-> ()] ["Foo" :--> "E", "Zoo" :--> "Foo"]
Rename ["Foo" :-> Int,"Bar" :-> ()] ["Foo" :--> "E", "Zoo" :--> "Foo"] :: [*]
= '["E" :-> Int, "Bar" :-> ()]

Как изменить код таким образом, чтобы такое переименование было запрещено (не выводилось)

> ρ :: RelSet a -> proxy b -> RelSet (Rename a b)
> ρ  r _ = unsafeCoerce r

\### Проекция

Проекцией называют

Как обычно начнем с функции работающей на уровне типов:

> type family Project' a b where
>   Project' s       '[]     = '[]
>   Project' s    (b ': bs)  = ProjectInner' s b ': Project' s bs 
>
> type family ProjectInner' s b where
>   ProjectInner' (s :-> a ': as) s = s :-> a
>   ProjectInner' (   a    ': as) s = ProjectInner' as s

Пример:

*Main> :kind! Project' ["Foo" :-> Int,"Bar" :-> ()] '["Foo"]
Project' ["Foo" :-> Int,"Bar" :-> ()] '["Foo"] :: [*]
= '["Foo" :-> Int]


Попробуем теперь построить решение другим способом:

> class Project s a b c | s a b -> c where
>   rproject :: s a -> Proxy b -> s c 
> 
> instance Project HRec a '[] '[] where
>   rproject _ _ = HNil
> 
> instance (KnownSymbol b, Project HRec a bs c, Lookup HRec a b k) => Project HRec a (b ': bs) ((b :-> k) ': c) where
>   rproject xs p = rcons (pHead p)
>                         (rlookupP xs (pHead p))
>                         (rproject xs (pTail p))
>     where pHead :: Proxy (b ': bs) -> Proxy b
>           pHead _ = Proxy
>           pTail :: Proxy (b ': bs) -> Proxy bs
>           pTail _ = Proxy


> rcons :: KnownSymbol b => Proxy b -> a -> HRec c -> HRec ( (b :-> a) ': c)
> rcons _ a xs = HCons a xs

Пример:

*Main> rproject t (Proxy :: Proxy '["Bar"])
HCons (Bar :-> ()) (HNil)


Теперь сделаем такую же операцию для множеств, заодно убрав прокси:


> π :: (Project HRec xs ys ys, Ord (HRec ys)) => RelSet xs -> RelSet ys
> π rs = inner Proxy rs 
>   where inner :: (Project HRec xs ys ys, Ord (HRec ys)) => Proxy ys -> RelSet xs -> RelSet ys
>         inner p (RS sx) = RS $ Set.map (\x -> rproject x p) sx

> projection :: (Project HRec xs ys ys, Ord (HRec ys)) => RelSet xs -> RelSet ys
> projection = π 

Пример: ????

\### Выборка

Для выборки придется немного пофантазировать, сперва мы введем специальный класс,
который позволяет объединять предикаты по полям в предикат по записи

> class RPredicate s a b where
>   rpredicate :: s a -> s b -> Bool

> instance RPredicate HRec s '[] where
>   rpredicate _ _ = True

> instance (RPredicate HRec s ps, RPredicate1 HRec s (n :-> (a -> Bool))) => RPredicate HRec s (( n :-> (a -> Bool)) ': ps) where
>   rpredicate s h@(HCons _ xs) = rpredicate1 s (v h) && rpredicate s xs
>     where v :: HRec (n :-> (a -> Bool) ': b) -> (n :-> (a -> Bool))
>           v (HCons f _) = V f


> class RPredicate1 s a b where
>   rpredicate1 :: s a -> b -> Bool

> instance RPredicate1 HRec ((s :-> a) ': as) (s :-> (a -> Bool)) where
>   rpredicate1 (HCons a _) (V f) = f a

> instance RPredicate1 HRec as f => RPredicate1 HRec (a ': as) f where
>   rpredicate1 (HCons a as) f = rpredicate1 as f 

> type family EqPred z where
>   EqPred '[] = '[]
>   EqPred ((n :-> a) ': xs) = (n :-> (a -> Bool)) ': EqPred xs

> class EqPredicate s a b | s a -> b where
>   eqPredicate :: s a -> s b

> instance EqPredicate HRec '[] '[] where
>   eqPredicate HNil = HNil

> instance (EqPredicate HRec as bs, Eq a) => EqPredicate HRec ((n :-> a) ': as) ((n :-> (a -> Bool)) ': bs) where
>   eqPredicate (HCons x xs) = HCons (==x) (eqPredicate xs)


> σ :: (RPredicate HRec xs ys) => RelSet xs -> HRec ys -> RelSet xs
> σ (RS sx) h = RS $ Set.filter (\x -> rpredicate x h) sx

> selection :: (RPredicate HRec xs ys) => RelSet xs -> HRec ys -> RelSet xs
> selection = σ

\### Умножение

> type family Append xs ys where
>   Append '[] ys = ys
>   Append (x ': xs) ys = x ': Append xs ys

> class Merge s a b c | s a b -> c where
>   rmerge :: s a -> s b -> s c
> 
> instance Merge HRec '[] b b where
>   rmerge HNil xs = xs
> 
> instance Merge HRec as b c => Merge HRec (a ': as) b (a ': c) where
>   rmerge (HCons x xs) s = HCons x (rmerge xs s)

> multiplication :: (Merge HRec xs ys (Append xs ys), Ord (HRec (Append xs ys))) => RelSet xs -> RelSet ys -> RelSet (Append xs ys)
> multiplication (RS sx) (RS sy) =
>   RS $ Set.fromList [ x `rmerge` y
>                     | x <- Set.toList sx
>                     , y <- Set.toList sy
>                     ]

\### Деление


> type family Minus xs ys where
>   Minus xs '[] = xs
>   Minus xs (y ': ys) = Minus (MinusInner xs y) ys

> type family MinusInner xs y where
>   MinusInner (y ': xs) y = xs
>   MinusInner (x ': xs) y = x ': MinusInner xs y

> division :: (Eq (RelSet ys), Ord (HRec ys), Project HRec xs ys ys, RPredicate HRec xs (EqPred (Minus xs ys)), EqPredicate HRec (Minus xs ys) (EqPred (Minus xs ys)), Ord (HRec (Minus xs ys)), Project HRec xs (Minus xs ys) (Minus xs ys)) => RelSet xs -> RelSet ys -> RelSet (Minus xs ys)
> division rx ry = RS $ Set.fromList [ s | (s,v) <- [ (x, projection (selection rx (eqPredicate x))) | x <- Set.toList (unRS $ projection rx)], v == ry]

> class Lookup s a b c | s a b -> c where
>   rlookup' :: Proxy a -> Proxy b -> s a -> c 
> 
> instance Lookup HRec ( (n :-> a) ': xs ) n a where
>   rlookup' _ _ (HCons a _) = a
> 
> instance Lookup HRec xs n c => Lookup HRec ( (m :-> a) ': xs ) n c where
>   rlookup' pa pb (HCons _ xs) = rlookup' (pa' pa) pb xs
>     where
>       pa' :: Proxy (a ': xs) -> Proxy xs
>       pa' _ = Proxy
> 
> rlookupP :: Lookup s a b c => s a -> Proxy b -> c
> rlookupP s b = rlookup' Proxy b s 

