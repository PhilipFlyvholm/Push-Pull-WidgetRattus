{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Behaviour where

import Primitives
import WidgetRattus
import WidgetRattus.Signal hiding (future, current, const, switch)
import Prelude hiding (const, map, zipWith)
import qualified Prelude

type SBeh a = Sig (Fun Time a)

newtype Beh a = Beh (Time -> SBeh a)

const :: Fun Time a -> Beh a
const x = Beh (\_ -> x ::: never) -- Not sure this is correct

timeBehaviour :: Beh Time
timeBehaviour = const (Fun (box id))

map :: forall a b. Box (a -> b) -> Beh a -> Beh b
map f (Beh as) =
  Beh (\t -> run (as t))
  where
    run :: SBeh a -> SBeh b
    run (x ::: xs) =
      mapF f x ::: delay (run (adv xs))

sampleInterval :: O ()
sampleInterval = timer 200000 -- For some reason is this a second

current :: Beh a -> Time -> Fun Time a
current (Beh f) t = 
  let (x ::: _) = f t in x

future :: Beh a -> Time -> O (SBeh a)
future (Beh f) t =
  let (_ ::: xs) = f t in xs

discretize :: Beh a -> C (Sig a)
discretize b = do
  t <- time
  let cur = current b t
  let fut = future b t
  let rest = delayC $ delay
            ( case select fut sampleInterval of
                Fst x _ -> discretize (Beh (Prelude.const x))
                Snd beh' _ -> discretize (Beh (\t -> (current b t) ::: beh'))
                Both x _ -> discretize (Beh (Prelude.const x))
            )
  return $ (apply cur t) ::: rest
{-
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

-- | This function is a variant of combines the values of two signals
-- using the function argument. @zipWith f xs ys@ produces a new value
-- @unbox f x y@ whenever @xs@ or @ys@ produce a new value, where @x@
-- and @y@ are the current values of @xs@ and @ys@, respectively.
--
-- Example:
--
-- >                      xs:  1 2 3     2
-- >                      ys:  1     0 5 2
-- >
-- > zipWith (box (+)) xs ys:  2 3 4 3 8 4
zipWith :: (Stable a, Stable b) => Box (a -> b -> c) -> Beh a -> Beh b -> Beh c
zipWith f (Beh (x ::: xs)) (Beh (y ::: ys)) =
  Beh
    ( app x y
        ::: delay
          ( let (Beh rest) =
                  ( case select xs ys of
                      Fst xs' lys -> Behaviour.zipWith f (Beh xs') (Beh (y ::: lys))
                      Snd lxs ys' -> Behaviour.zipWith f (Beh (x ::: lxs)) (Beh ys')
                      Both xs' ys' -> Behaviour.zipWith f (Beh xs') (Beh ys')
                  )
             in rest
          )
    )
  where
    app (K x') (K y') = K (unbox f x' y')
    app (Fun x') (Fun y') = Fun (box (\t -> unbox f (unbox x' t) (unbox y' t)))
    app (Fun x') (K y') = Fun (box (\t -> unbox f (unbox x' t) y'))
    app (K x') (Fun y') = Fun (box (unbox f x' . unbox y'))

-- | Variant of 'zipWith' with three behaviours.
zipWith3 :: forall a b c d. (Stable a, Stable b, Stable c) => Box (a -> b -> c -> d) -> Beh a -> Beh b -> Beh c -> Beh d
zipWith3 f as bs cs = Behaviour.zipWith (box (\f' x -> unbox f' x)) cds cs
  where
    cds :: Beh (Box (c -> d))
    cds = Behaviour.zipWith (box (\a b -> box (\c -> unbox f a b c))) as bs
    -}