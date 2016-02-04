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

Тут все по старому

> data TaggedOrder = TaggedOrder
>   { getOrder :: Order
>   , origOrder :: Order
>   , isSent   :: Bool
>   , isPrepaid :: Bool
>   , isRisky   :: Bool
>   }

> withOrder :: TaggedOrder -> (Order -> Order) -> TaggedOrder
> withOrder t f = t{ getOrder = f (getOrder t) }


> data Prop = IsPrepaid | IsSent | IsRisk | Not Prop

Новое свойство, разрешено ли конфигурацией обновление (компиляется)

> data OrderProp = Increase | Decrease | NotValid | NotAllowed

> data Rule (p :: [Prop]) (o :: [OrderProp]) (a :: Action)

А вот действия новые появились:

> data Action = Deny -- ^ Отменить изменение
>             | Pass -- ^ Разрешить изменение
>             | Auth Action Action  -- ^ Запрос авторизации в банк (всегда на разницу)
>             | Capture Action Action -- ^ Capture ?
>             | RiskCheck Action Action -- ^ сделать RiskCheck
>             deriving (Eq, Show)

компилятор радостно сказал, что есть упущенные случае в инстансе Monoid и в интерпретаторе

> instance Monoid Action where
>   mempty = Pass
>   mappend Deny _ = Deny
>   mappend _    Deny = Deny
>   mappend (Auth a b) c = Auth (a <> c) (b <> c)
>   mappend (Capture a b) c = Capture (a <> c) (b <> c)
>   mappend (RiskCheck a b) c = RiskCheck (a `mappend` c) (b `mappend` c)
>   mappend Pass x = x

После исправления инстанса и интерпретатора можно заняться правилами:

> type Rules1 = [ Rule '[ 'IsPrepaid] '[ 'Increase, 'NotAllowed] 'Deny -- если заказ предоплачен (неважно, отправлен или нет),
>                                                                     -- можно увеличивать сумму, если это разрешено конфигурацией магазина.
>               , Rule '[ 'IsPrepaid, 'Not 'IsSent] '[ 'Increase] ('Auth 'Deny 'Pass) -- если заказ предоплачен, неотправлен, и сумма увеличивается,
>                                                                       -- надо сделать auth-запрос в банк. если он не срабатывает, увеличить нельзя.
>               , Rule '[ 'IsPrepaid, 'IsSent] '[ 'Increase] ('Auth 'Deny ('Capture 'Deny 'Pass))
>                                                                       -- если заказ предоплачен, отправлен, и сумма увеличивается,
>                                                                       -- надо сделать auth-запрос в банк на разницу в сумме,
>                                                                       -- а потом сделать capture запрос на всю сумму. Если хоть один из них не срабатывает, увеличить нельзя.
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
> interpret (Auth no ok) to d = do
>    b <- performBankAuth to d
>    if b then interpret ok to d
>         else interpret no to d
> interpret (Capture no ok) to d = do
>    eto <- interpret ok to d -- а что же у нас в случае успеха
>    case eto of -- тут компилятор отловил ошибки типов
>      Left e -> return (Left e)
>      Right to' -> do b <- performCapture to'
>                      if b then return (Right to')
>                           else interpret no to d
> interpret (RiskCheck no ok) to d = do
>    b <- performRiskCheck (getOrder to)
>    if b then interpret ok to d
>         else interpret no to d

> performRiskCheck :: Order -> IO Bool
> performRiskCheck = undefined

> performBankAuth :: TaggedOrder -> Double -> IO Bool
> performBankAuth = undefined

> performCapture :: TaggedOrder -> IO Bool
> performCapture = undefined

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
> instance (MkAction a, MkAction b) => MkAction ('Auth a b) where
>   mkAction _ = Auth (mkAction (Proxy :: Proxy a)) (mkAction (Proxy :: Proxy b))
> instance (MkAction a, MkAction b) => MkAction ('Capture a b) where
>   mkAction _ = Capture (mkAction (Proxy :: Proxy a)) (mkAction (Proxy :: Proxy b))



> class HasOrderTags c where hasOrderTags :: proxy c -> TaggedOrder -> Bool
> instance HasOrderTags '[] where hasOrderTags _ _ = True
> instance HasOrderTags ps => HasOrderTags ('IsPrepaid ': ps) where 
>   hasOrderTags _ to = isPrepaid to && hasOrderTags (Proxy :: Proxy ps) to
> instance HasOrderTags ps => HasOrderTags ('IsSent ': ps) where
>   hasOrderTags _ to = isSent to && hasOrderTags (Proxy :: Proxy ps) to
> instance HasOrderTags ps => HasOrderTags ('IsRisk ': ps) where
>   hasOrderTags _ to = isRisky to && hasOrderTags (Proxy :: Proxy ps) to
> instance HasOrderTags ps => HasOrderTags ('Not 'IsSent ': ps) where
>   hasOrderTags _ to = not (isSent to) && hasOrderTags (Proxy :: Proxy ps) to

Но добавить эту проверку не так и просто, нужны данные, можно сделать через Reader, но мне
лень и я люблю явные агрументы пока они совсем уж не достанут. Ещё можно всякие имплиситы,
но пофиг.

> data Config = Config 
>  { configMaxIncrease :: Double
>  , configMaxDecrease :: Double
>  , configRateIncrease :: Double
>  , configRateDecrease :: Double
>  , configAllowedPrepaidIncrease :: Bool
>  }



> class HasPaymentTags c where hasPaymentTags :: proxy c -> Config -> TaggedOrder -> Double -> Bool
> instance HasPaymentTags '[] where hasPaymentTags _ _ _ _ = True
> instance HasPaymentTags ps => HasPaymentTags ('Increase ': ps) where
>   hasPaymentTags _ c t d = d >= 0 && hasPaymentTags (Proxy :: Proxy ps) c t d
> instance HasPaymentTags ps => HasPaymentTags ('Decrease ': ps) where
>   hasPaymentTags _ c t d = d < 0 && hasPaymentTags (Proxy :: Proxy ps) c t d
> instance HasPaymentTags ps => HasPaymentTags ('NotValid ': ps) where
>   hasPaymentTags _ c t d
>     | d >= 0 = d >= ((configMaxIncrease c) `max` (configRateIncrease c * origOrder t)) && hasPaymentTags (Proxy :: Proxy ps) c t d
>     | otherwise = (-d) >= ((configMaxDecrease c) `max` (configRateDecrease c * origOrder t)) && hasPaymentTags (Proxy :: Proxy ps) c t d
> instance HasPaymentTags ps => HasPaymentTags ('NotAllowed ': ps) where
>   hasPaymentTags _ c t d = configAllowedPrepaidIncrease c && hasPaymentTags (Proxy :: Proxy ps) c t d

Сделали, а типы в инстансе MkRules не сошлись, возвращаемся исправляем, говорит
параметров недодали. Исправили, все работает, можно и проверить


> step1Rules :: Proxy Rules1
> step1Rules = Proxy

> rules :: Config -> TaggedOrder -> Double -> Action
> rules = mkRules step1Rules

