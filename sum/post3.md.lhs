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
интересных тегов. Оказывается ещё и огиринальную сумму хранить надо, но
ничего, прямого доступа к TaggedOrder нету, так что можно туда положить.
(после изменени все компиляется).

> data TaggedOrder = TaggedOrder
>   { getOrder :: Order
>   , origOrder :: Order
>   , isSent   :: Bool
>   , isPrepaid :: Bool
>   , isRisky   :: Bool
>   }

> withOrder :: TaggedOrder -> (Order -> Order) -> TaggedOrder
> withOrder t f = t{ getOrder = f (getOrder t) }

Интересные нам свойства. Свойства так и не поменялись, хорошо.

> data Prop = IsPrepaid | IsSent | IsRisk

Свойства изменения оплаты теперь поменялись, оказывается, есть ещё интересные
свойства нужено проверять валидность изменения, добавляем. (все ещё компиляется)

> data OrderProp = Increase | Decrease | NotValid

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

Тут нам нужно добавить ещё одно правило, хорошо что можно просто в конец.
(все ещё компиляется, как же так!)

> type Rules1 = [ Rule '[ 'IsPrepaid, 'IsSent] '[]  'Deny
>               , Rule '[ 'IsPrepaid] '[ 'Decrease] 'Deny
>               , Rule '[]            '[ 'Increase] ('RiskCheck 'Deny 'Pass)
>               , Rule '[ 'IsRisk]              '[] 'Deny
>               , Rule '[]            '[ 'NotValid] 'Deny
>               ]

На самом деле пример перестал работать и код, осуществлящий создание правила упадёт:

<interactive>:8:1:
    No instance for (HasPaymentTags '['NotValid])
    arising from a use of ‘mkRules’
    In the expression:
      mkRules step1Rules (TaggedOrder 100 100 True False False) (- 15)
    In an equation for ‘it’:
      it
        = mkRules step1Rules (TaggedOrder 100 100 True False False) (- 15)


Отлично, я знаю теперь чего не хватает :)

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
>   mkRules :: proxy c -> Config -> TaggedOrder -> Double -> Action


> instance MkRules '[] where
>   mkRules _ _ _ _ = Pass

> instance (HasPaymentTags i, HasOrderTags s, MkAction a, MkRules rs) => MkRules (Rule s i a ': rs) where
>   mkRules _ c t d 
>     | hasOrderTags (Proxy :: Proxy s) t && hasPaymentTags (Proxy :: Proxy i) c t d
>                 = mkAction (Proxy :: Proxy a) <> mkRules (Proxy :: Proxy rs) c t d
>     | otherwise = mkRules (Proxy :: Proxy rs) c t d

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

Но добавить эту проверку не так и просто, нужны данные, можно сделать через Reader, но мне
лень и я люблю явные агрументы пока они совсем уж не достанут. Ещё можно всякие имплиситы,
но пофиг.

> data Config = Config 
>  { configMaxIncrease :: Double
>  , configMaxDecrease :: Double
>  , configRateIncrease :: Double
>  , configRateDecrease :: Double
>  }



> class HasPaymentTags c where hasPaymentTags :: proxy c -> Config -> TaggedOrder -> Double -> Bool
> instance HasPaymentTags '[] where hasPaymentTags _ _ _ _ = True
> instance HasPaymentTags ps => HasPaymentTags ('Increase ': ps) where
>   hasPaymentTags _ c t d = d >= 0 && hasPaymentTags (Proxy :: Proxy ps) c t d
> instance HasPaymentTags ps => HasPaymentTags ('Decrease ': ps) where
>   hasPaymentTags _ c t d = d < 0 && hasPaymentTags (Proxy :: Proxy ps) c t d
> instance HasPaymentTags ps => HasPaymentTags ('NotValid ': ps) where
>   hasPaymentTags _ c t d
>     | d >= 0 = d >= (configMaxIncrease c) `max` (configRateIncrease c * origOrder t)
>     | otherwise = (-d) >= (configMaxDecrease c) `max` (configRateDecrease c * origOrder t)

Сделали, а типы в инстансе MkRules не сошлись, возвращаемся исправляем, говорит
параметров недодали. Исправили, все работает, можно и проверить


> step1Rules :: Proxy Rules1
> step1Rules = Proxy

> rules :: Config -> TaggedOrder -> Double -> Action
> rules = mkRules step1Rules

