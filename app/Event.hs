{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Event where

import Behaviour
import Primitives (Fun (..), apply, mapF)
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

map2 :: Box (a -> b) -> Ev a -> Ev b
map2 f (Ev sig) = Ev (delay (WidgetRattus.Signal.map f (adv sig)))

map3 :: Box (a -> b) -> Ev a -> Ev b
map3 f (Ev sig) = Ev (aux f sig)
  where
    aux :: Box (a -> b) -> O (Sig a) -> O (Sig b)
    aux f xs = delay (let (x ::: xs') = adv xs in unbox f x ::: aux f xs')

stepper :: a -> Ev a -> Beh a
stepper initial event =
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
interleave f (Ev xs) (Ev ys) = Ev (aux f xs ys)
  where
    aux :: Box (a -> a -> a) -> O (Sig a) -> O (Sig a) -> O (Sig a)
    aux f xs ys =
      delay 
        ( case select xs ys of
            Fst (x ::: xs') ys' -> x ::: aux f xs' ys'
            Snd xs' (y ::: ys') -> y ::: aux f xs' ys'
            Both (x ::: xs') (y ::: ys') -> unbox f x y ::: aux f xs' ys'
        )

scan :: (Stable b) => Box (b -> a -> b) -> b -> Ev a -> Ev b
scan f acc (Ev as) = Ev $ delay (WidgetRattus.Signal.scan f acc (adv as))

app :: (Stable b, Stable a) => Box (b -> a -> b) -> Box (Fun b -> a -> Fun b)
app f =
  box
    (\b a -> mapF (box (\b' -> unbox f b' a)) b)

scanB :: (Stable b, Stable a) => Box (b -> a -> b) -> b -> Ev a -> Beh b
scanB f acc (Ev as) =
  Beh (K acc ::: delay (WidgetRattus.Signal.scan (app f) (K acc) (adv as)))
