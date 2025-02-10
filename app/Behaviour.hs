{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Behaviour where

import Primitives
import WidgetRattus
import WidgetRattus.Signal hiding (const, switch, zipWith)
import Prelude hiding (const, map, zipWith)

newtype Beh a = Beh (Sig (Fun Time a))

const :: Fun Time a -> Beh a
const x = Beh (x ::: never)

timeBehaviour :: Beh Time
timeBehaviour = const (Fun (box id))

map :: Box (a -> b) -> Beh a -> Beh b
map f (Beh (x ::: xs)) = Beh (mapF f x ::: delay (let Beh s = Behaviour.map f (Beh (adv xs)) in s))

sampleInterval :: O ()
sampleInterval = timer 200000 -- For some reason is this a second

discretize :: Beh a -> C (Sig a)
discretize (Beh (K x ::: xs)) = do
  let rest = delayC $ delay (let x' = adv xs in discretize (Beh x'))
  return $ x ::: rest
discretize (Beh (Fun f ::: xs)) = do
  t <- time
  let cur = unbox f t
  let rest =
        delayC $
          delay
            ( case select xs sampleInterval of
                Fst x _ -> discretize (Beh x)
                Snd beh' _ -> discretize (Beh (Fun f ::: beh'))
                Both x _ -> discretize (Beh x)
            )
  return (cur ::: rest)

elapsedTime :: C (Beh NominalDiffTime)
elapsedTime = do
  startTime <- time
  return $ Beh (Fun (box (\currentTime -> diffTime currentTime startTime)) ::: never)

withTime :: O (Time -> a) -> O a
withTime delayed =
  delayC $ delay (let f = adv delayed in do f <$> time)

switch :: Beh a -> O (Beh a) -> Beh a
switch (Beh (x ::: xs)) d =
  Beh $
    x
      ::: delay
        ( case select xs d of
            Fst xs' d' -> let (Beh b') = Behaviour.switch (Beh xs') d' in b'
            Snd _ (Beh d') -> d'
            Both _ (Beh d') -> d'
        )

zipWith :: (Stable a, Stable b) => Box (a -> b -> c) -> Beh a -> Beh b -> Beh c
zipWith f (Beh (x ::: xs)) (Beh (y ::: ys)) =
  Beh
    ( app x y
        ::: delay
          ( let (Beh rest) =
                  ( case select xs ys of
                      Fst xs' lys -> zipWith f (Beh xs') (Beh (y ::: lys))
                      Snd lxs ys' -> zipWith f (Beh (x ::: lxs)) (Beh ys')
                      Both xs' ys' -> zipWith f (Beh xs') (Beh ys')
                  )
             in rest
          )
    )
  where
    app (K x') (K y') = K (unbox f x' y')
    app (Fun x') (Fun y') = Fun (box (\t -> unbox f (unbox x' t) (unbox y' t)))
    app (Fun x') (K y') = Fun (box (\t -> unbox f (unbox x' t) y'))
    app (K x') (Fun y') = Fun (box (unbox f x' . unbox y'))