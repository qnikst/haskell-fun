> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# OPTIONS_GHC -Wall #-}

> import Data.Proxy
> import Data.Monoid

Наш заказ как он есть, без всякой доп инфы

> type Order = Double

Заказ, как он хранится в базе, где на него могут навешать всяких
интересных тегов.

> data TaggedOrder = TaggedOrder
>   { getOrder :: Order
>   , isSent   :: Bool
>   , isPrepaid :: Bool
>   , isRisky   :: Bool
>   }

> withOrder :: TaggedOrder -> (Order -> Order) -> TaggedOrder
> withOrder t f = t{ getOrder = f (getOrder t) }

Интересные нам свойства

> data Prop = IsPrepaid | IsSent | IsRisk

Свойства изменения оплаты

> data OrderProp = Increase | Decrease

Попробуем красиво записать правила:

> data Rule (p :: [Prop]) (o :: [OrderProp]) (a :: Action)

> data Action = Deny -- ^ Отменить изменение
>             | Pass -- ^ Разрешить изменение
>             | RiskCheck Action Action -- ^ сделать RiskCheck
>             deriving (Eq, Show)

> instance Monoid Action where
>   mempty = Pass
>   mappend Deny _ = Deny
>   mappend _    Deny = Deny
>   mappend (RiskCheck a b) c = RiskCheck (a `mappend` c) (b `mappend` c)
>   mappend Pass x = x

> type Rules1 = [ Rule '[ 'IsPrepaid, 'IsSent] '[]  'Deny
>               , Rule '[ 'IsPrepaid] '[ 'Decrease] 'Deny
>               , Rule '[]            '[ 'Increase] ('RiskCheck 'Deny 'Pass)
>               , Rule '[ 'IsRisk]              '[] 'Deny
>               ]

Данное решение не совсем честное, т.к. на самом деле перестанет работать в случае,
если у нас появятся модификаторы тегов, а они точно появятся.

> interpret :: Action -> TaggedOrder -> Double -> IO (Either String TaggedOrder)
> interpret Pass  to  d = return $ pure (withOrder to (+d))
> interpret Deny _to _d = return $ fail "Order was blocked"
> interpret (RiskCheck no ok) to d = do
>    b <- performRiskCheck (getOrder to)
>    if b then interpret ok to d
>         else interpret no to d

> performRiskCheck :: Order -> IO Bool
> performRiskCheck = undefined


> class MkRules (c :: k) where
>   mkRules :: proxy c -> TaggedOrder -> Double -> Action


> instance MkRules '[] where
>   mkRules _ _ _ = Pass

> instance (HasPaymentTags i, HasOrderTags s, MkAction a, MkRules rs) => MkRules (Rule s i a ': rs) where
>   mkRules _ t d 
>     | hasOrderTags (Proxy :: Proxy s) t && hasPaymentTags (Proxy :: Proxy i) d
>                 = mkAction (Proxy :: Proxy a) <> mkRules (Proxy :: Proxy rs) t d
>     | otherwise = mkRules (Proxy :: Proxy rs) t d

> class MkAction (a::Action) where mkAction :: proxy a -> Action
> instance MkAction 'Pass where mkAction _ = Pass
> instance MkAction 'Deny where mkAction _ = Deny
> instance (MkAction a, MkAction b) => MkAction ('RiskCheck a b) where
>   mkAction _ = RiskCheck (mkAction (Proxy :: Proxy a)) (mkAction (Proxy :: Proxy b))


> class HasOrderTags c where hasOrderTags :: proxy c -> TaggedOrder -> Bool
> instance HasOrderTags '[] where hasOrderTags _ _ = True
> instance HasOrderTags ps => HasOrderTags ('IsPrepaid ': ps) where 
>   hasOrderTags _ to = isPrepaid to && hasOrderTags (Proxy :: Proxy ps) to
> instance HasOrderTags ps => HasOrderTags ('IsSent ': ps) where
>   hasOrderTags _ to = isSent to && hasOrderTags (Proxy :: Proxy ps) to
> instance HasOrderTags ps => HasOrderTags ('IsRisk ': ps) where
>   hasOrderTags _ to = isRisky to && hasOrderTags (Proxy :: Proxy ps) to

> class HasPaymentTags c where hasPaymentTags :: proxy c -> Double -> Bool
> instance HasPaymentTags '[] where hasPaymentTags _ _ = True
> instance HasPaymentTags ps => HasPaymentTags ('Increase ': ps) where
>   hasPaymentTags _ d = d >= 0 && hasPaymentTags (Proxy :: Proxy ps) d
> instance HasPaymentTags ps => HasPaymentTags ('Decrease ': ps) where
>   hasPaymentTags _ d = d < 0 && hasPaymentTags (Proxy :: Proxy ps) d


> step1Rules :: Proxy Rules1
> step1Rules = Proxy

*Main> mkRules step1Rules (TaggedOrder 100 True True True) 15
Deny
*Main> mkRules step1Rules (TaggedOrder 100 True True False) 15
Deny
*Main> mkRules step1Rules (TaggedOrder 100 True False False) 15
RiskCheck Deny Pass
*Main> mkRules step1Rules (TaggedOrder 100 True False False) (-15)
Pass
