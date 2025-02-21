{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Event where

import Behaviour
import Data.IntMap ()
import Primitives (Fun (..), apply, mapF)
import WidgetRattus
import WidgetRattus.Signal hiding (interleave, scan)

data Ev a
  = EvDense !(O (Sig a))
  | EvSparse !(O (Sig (Maybe' a)))

mkEv :: Box (O a) -> Ev a
mkEv a =
  EvDense
    ( delay
        ( adv (unbox a)
            ::: let EvDense s = mkEv a
                 in s
        )
    )

mapMaybe :: Box (a -> b) -> Maybe' a -> Maybe' b
mapMaybe f (Just' a) =
  Just' $ unbox f a
mapMaybe _ Nothing' = Nothing'

map :: Box (a -> b) -> Ev a -> Ev b
map f (EvDense sig) =
  EvDense
    ( delay
        ( let (x ::: xs) = adv sig
           in unbox f x
                ::: let (EvDense rest) = Event.map f (EvDense xs)
                     in rest
        )
    )
map f (EvSparse sig) =
  EvSparse
    ( delay
        ( let (x ::: xs) = adv sig
           in mapMaybe f x
                ::: let (EvSparse rest) = Event.map f (EvSparse xs)
                     in rest
        )
    )

stepper :: a -> Ev a -> Beh a
stepper initial event =
  Beh (K initial ::: delay (let (Beh sig) = adv (stepperAwait event) in sig))

stepperAwait :: Ev a -> O (Beh a)
stepperAwait (EvDense sig) =
  delay (let (x ::: xs) = adv sig in Beh (K x ::: delay (let (Beh sig') = adv (stepperAwait (EvDense xs)) in sig')))


triggerAwait :: (Stable b) => Box (a -> b -> c) -> Ev a -> Beh b -> Ev c
triggerAwait f event behaviour = EvSparse (trig f event behaviour)
  where
    trig :: (Stable b) => Box (a -> b -> c) -> Ev a -> Beh b -> O (Sig (Maybe' c))
    trig f' (EvSparse as) (Beh (b ::: bs)) =
      delayC $
        delay
          ( let d = select as bs
             in ( do
                    t <- time
                    return
                      ( case d of
                          Fst (a' ::: as') bs' ->
                            let rest = trig f' (EvSparse as') (Beh (b ::: bs'))
                             in case a' of
                                  Just' a'' -> Just' (unbox f' a'' (apply b t)) ::: rest
                                  Nothing' -> Nothing' ::: rest
                          Snd as' bs' -> Nothing' ::: trig f' (EvSparse as') (Beh bs')
                          Both (a' ::: as') (b' ::: bs') ->
                            let rest = trig f' (EvSparse as') (Beh (b' ::: bs'))
                             in case a' of
                                  Just' a'' -> Just' (unbox f' a'' (apply b' t)) ::: rest
                                  Nothing' -> Nothing' ::: rest
                      )
                )
          )
    trig f' (EvDense as) (Beh (b ::: bs)) =
      delayC $
        delay
          ( let d = select as bs
             in ( do
                    t <- time
                    return
                      ( case d of
                          Fst (a' ::: as') bs' -> Just' (unbox f' a' (apply b t)) ::: trig f' (EvDense as') (Beh (b ::: bs'))
                          Snd as' bs' -> Nothing' ::: trig f' (EvDense as') (Beh bs')
                          Both (a' ::: as') (b' ::: bs') -> Just' (unbox f' a' (apply b' t)) ::: trig f' (EvDense as') (Beh (b' ::: bs'))
                      )
                )
          )

interleave :: Box (a -> a -> a) -> Ev a -> Ev a -> Ev a
interleave f (EvDense xs) (EvDense ys) =
  EvDense $
    delay
      ( case select xs ys of
          Fst (x ::: xs') ys' ->
            let EvDense rest = interleave f (EvDense xs') (EvDense ys')
             in (x ::: rest)
          Snd xs' (y ::: ys') ->
            let EvDense rest = interleave f (EvDense xs') (EvDense ys')
             in (y ::: rest)
          Both (x ::: xs') (y ::: ys') ->
            let EvDense rest = interleave f (EvDense xs') (EvDense ys')
             in unbox f x y ::: rest
      )
interleave f (EvSparse xs) (EvSparse ys) =
  EvSparse $
    delay
      ( case select xs ys of
          Fst (x ::: xs') ys' ->
            let EvSparse rest = interleave f (EvSparse xs') (EvSparse ys')
             in (x ::: rest)
          Snd xs' (y ::: ys') ->
            let EvSparse rest = interleave f (EvSparse xs') (EvSparse ys')
             in (y ::: rest)
          Both (x ::: xs') (y ::: ys') ->
            let EvSparse rest = interleave f (EvSparse xs') (EvSparse ys')
             in case x :* y of
                  (Just' x' :* Just' y') ->
                    Just' (unbox f x' y') ::: rest
                  (_ :* _) ->
                    Nothing' ::: rest
      )
interleave f (EvSparse xs) (EvDense ys) =
  EvSparse $
    delay
      ( case select xs ys of
          Fst (x ::: xs') ys' ->
            let EvSparse rest = interleave f (EvSparse xs') (EvDense ys')
             in (x ::: rest)
          Snd xs' (y ::: ys') ->
            let EvSparse rest = interleave f (EvSparse xs') (EvDense ys')
             in (Just' y ::: rest)
          Both (x ::: xs') (y ::: ys') ->
            let EvSparse rest = interleave f (EvSparse xs') (EvDense ys')
             in case x of
                  (Just' x') ->
                    Just' (unbox f x' y) ::: rest
                  _ ->
                    Nothing' ::: rest
      )
interleave f (EvDense xs) (EvSparse ys) =
  EvSparse $
    delay
      ( case select xs ys of
          Fst (x ::: xs') ys' ->
            let EvSparse rest = interleave f (EvDense xs') (EvSparse ys')
             in (Just' x ::: rest)
          Snd xs' (y ::: ys') ->
            let EvSparse rest = interleave f (EvDense xs') (EvSparse ys')
             in (y ::: rest)
          Both (x ::: xs') (y ::: ys') ->
            let EvSparse rest = interleave f (EvDense xs') (EvSparse ys')
             in case y of
                  (Just' y') ->
                    Just' (unbox f x y') ::: rest
                  _ ->
                    Nothing' ::: rest
      )

scan :: (Stable b) => Box (b -> a -> b) -> b -> Ev a -> Ev b
scan f acc (EvDense ev) =
  EvDense $
    delay
      ( let (x ::: xs) = adv ev
            acc' = unbox f acc x
            EvDense rest = scan f acc' (EvDense xs)
            in acc' ::: rest
      )
scan f acc (EvSparse ev) =
  EvSparse $
  delay
    ( let (x ::: xs) = adv ev
      in case x of
        Just' x' -> 
          let acc' = unbox f acc x'
              EvSparse rest = scan f acc' (EvSparse xs)
              in Just' acc' ::: rest
        Nothing' -> 
          let EvSparse rest = scan f acc (EvSparse xs)
          in Just' acc ::: rest
    )