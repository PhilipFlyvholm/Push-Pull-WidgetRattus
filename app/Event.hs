{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
module Event where

import Behaviour
import Primitives (Fun (K), apply)
import WidgetRattus
import WidgetRattus.Signal hiding (interleave)

newtype Ev a = Ev (O (Sig a))

mkEv :: Box (O a) -> Ev a
mkEv a =
  Ev
    ( delay
        ( adv (unbox a)
            ::: let Ev s = mkEv a
                 in s
        )
    )

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

interleave :: Box (a -> a -> a) -> Ev a -> Ev a -> Ev a
interleave f (Ev xs) (Ev ys) =
  Ev $
    delay
      ( case select xs ys of
          Fst (x ::: xs') ys' ->
            let Ev rest = interleave f (Ev xs') (Ev ys')
             in (x ::: rest)
          Snd xs' (y ::: ys') ->
            let Ev rest = interleave f (Ev xs') (Ev ys')
             in (y ::: rest)
          Both (x ::: xs') (y ::: ys') ->
            let Ev rest = interleave f (Ev xs') (Ev ys')
             in unbox f x y ::: rest
      )

scan :: (Stable b) => Box (b -> a -> b) -> b -> Ev a -> Ev b
scan f acc (Ev as) = Ev $ delay (WidgetRattus.Signal.scan f acc (adv as))