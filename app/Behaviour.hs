{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Behaviour where

import Primitives
import WidgetRattus
import WidgetRattus.Signal
import Prelude hiding (map)

newtype Beh a = Beh (Sig (Fun Time a))

timeBehaviour :: Beh Time
timeBehaviour = Beh (Fun (box id) ::: never)

map :: Box (a -> b) -> Beh a -> Beh b
map f (Beh (x ::: xs)) = Beh (mapF f x ::: delay (let Beh s = Behaviour.map f (Beh (adv xs)) in s))

sampleInterval :: O ()
sampleInterval = timer 200000 -- For some reason is this a second

discretize :: Beh a -> C (Sig a)
discretize (Beh (K x ::: xs)) = do
  let rest = delayC $ delay (let x' = adv xs in discretize (Beh x'))
  return $ x ::: rest
discretize (Beh (Fun f ::: xs)) = do
  let dt = sampleInterval
  t <- time
  let cur = unbox f t
  let rest =
        delayC
          ( delay
              ( let d = select xs dt
                 in case d of
                      Fst x _ -> discretize (Beh x)
                      Snd beh' _ -> discretize (Beh (Fun f ::: beh'))
                      Both x _ -> discretize (Beh x)
              )
          )
  return (cur ::: rest)

elapsedTime :: C (Beh NominalDiffTime)
elapsedTime = do
  startTime <- time
  return $ Beh (Fun (box (\currentTime -> diffTime currentTime startTime)) ::: never)

withTime :: O (Time -> a) -> O a
withTime delayed =
  delayC $ delay (let f = adv delayed in do f <$> time)

triggerAwait :: (Stable b) => Box (a -> b -> c) -> O (Sig a) -> Beh b -> O (Sig (Maybe' c))
triggerAwait = trig
  where
    trig :: (Stable b) => Box (a -> b -> c) -> O (Sig a) -> Beh b -> O (Sig (Maybe' c))
    trig f' as (Beh (b ::: bs)) =
      delayC $
        delay
          ( let d = select as bs
             in ( do
                    t <- time
                    return
                      ( case d of
                          Fst (a' ::: as') bs' -> Just' (unbox f' a' (apply b t)) ::: trig f' as' (Beh (b ::: bs'))
                          Snd as' bs' -> Nothing' ::: trig f' as' (Beh bs')
                          Both (a' ::: as') (b' ::: bs') -> Just' (unbox f' a' (apply b' t)) ::: trig f' as' (Beh (b' ::: bs'))
                      )
                )
          )