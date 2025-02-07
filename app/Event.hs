{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
module Event where

import Behaviour
import Primitives (Fun (K))
import WidgetRattus
import WidgetRattus.Signal

newtype Ev a = Ev (O (Sig a))

map :: Box (a -> b) -> Ev a -> Ev b
map f (Ev sig) =
  Ev
    ( delay
        ( let (x ::: xs) = adv sig
           in unbox f x
                ::: let (Ev rest) = Event.map f (Ev xs)
                     in rest
        )
    )

stepper :: Ev a -> a -> Beh a
stepper event initial =
  Beh (K initial ::: delay (let (Beh sig) = adv (stepperAwait event) in sig))

stepperAwait :: Ev a -> O (Beh a)
stepperAwait (Ev sig) =
  delay (let (x ::: xs) = adv sig in Beh (K x ::: delay (let (Beh sig') = adv (stepperAwait (Ev xs)) in sig')))

{-
triggerAwait :: (Stable b) => Box (a -> b -> c) -> Ev a -> Beh b -> Ev (Maybe' c)
triggerAwait f event behaviour = Ev (trig f event behaviour)
  where
    trig :: (Stable b) => Box (a -> b -> c) -> Ev a -> Beh b -> O (Sig (Maybe' c))
    trig f' (Ev as) (Beh (b ::: bs)) =
      delayC $
        delay
          ( let d = select as bs
             in ( do
                    t <- time
                    return
                      ( case d of
                          Fst (a' ::: as') bs' -> Just' (unbox f' a' (apply b t)) ::: trig f' (Ev as') (Beh (b ::: bs'))
                          Snd as' bs' -> Nothing' ::: trig f' (Ev as') (Beh bs')
                          Both (a' ::: as') (b' ::: bs') -> Just' (unbox f' a' (apply b' t)) ::: trig f' (Ev as') (Beh (b' ::: bs'))
                      )
                )
          )
-}