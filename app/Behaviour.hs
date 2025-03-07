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
import WidgetRattus.InternalPrimitives (Continuous (..), O (Delay), adv', clockUnion, inputInClock)
import WidgetRattus.Signal hiding (const, jump, switch)
import Prelude hiding (const, map, zipWith)

newtype Beh a = Beh (Sig (Fun Time a))

unwrap :: Beh a -> Sig (Fun Time a)
unwrap (Beh a) = a

const :: Fun Time a -> Beh a
const x = Beh (x ::: never)

constK :: a -> Beh a
constK x = Beh (K x ::: never)

timeBehaviour :: Beh Time
timeBehaviour = const (Fun (box (\t -> t :* False)))

map :: Box (a -> b) -> Beh a -> Beh b
map f (Beh (x ::: xs)) = Beh (mapF f x ::: delay (unwrap $ Behaviour.map f (Beh (adv xs))))

sampleInterval :: O ()
sampleInterval = timer 20000

discretize :: Beh a -> C (Sig a)
discretize (Beh (K x ::: xs)) = do
  let rest = delayC $ delay (let x' = adv xs in discretize (Beh x'))
  return $ x ::: rest
discretize (Beh (Fun f ::: xs)) = do
  t <- time
  let (cur :* b) = unbox f t

  let rest =
        if b
          then never
          else
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
  return $ Beh (Fun (box (\currentTime -> diffTime currentTime startTime :* False)) ::: never)

withTime :: O (Time -> a) -> O a
withTime delayed =
  delayC $ delay (let f = adv delayed in do f <$> time)

switch :: Beh a -> O (Beh a) -> Beh a
switch (Beh (x ::: xs)) d =
  Beh $
    x
      ::: delay
        ( case select xs d of
            Fst xs' d' -> unwrap $ Behaviour.switch (Beh xs') d'
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
    app (Fun x') (Fun y') =
      Fun
        ( box
            ( \t ->
                let (a :* ab) = unbox x' t
                    (b :* bb) = unbox y' t
                    left = unbox f a b
                 in left :* ab || bb
            )
        )
    app (Fun x') (K y') =
      Fun
        ( box
            ( \t ->
                let (a :* ab) = unbox x' t
                    left = unbox f a y'
                 in left :* ab
            )
        )
    app (K x') (Fun y') =
      Fun
        ( box
            ( \t ->
                let (b :* bb) = unbox y' t
                    left = unbox f x' b
                 in left :* bb
            )
        )

-- | Variant of 'zipWith' with three behaviours.
zipWith3 :: forall a b c d. (Stable a, Stable b, Stable c) => Box (a -> b -> c -> d) -> Beh a -> Beh b -> Beh c -> Beh d
zipWith3 f as bs cs = Behaviour.zipWith (box (\f' x -> unbox f' x)) cds cs
  where
    cds :: Beh (Box (c -> d))
    cds = Behaviour.zipWith (box (\a b -> box (\c -> unbox f a b c))) as bs

stop :: Box (a -> Bool) -> Beh a -> Beh a
stop p (Beh b) = Beh (run b)
  where
    run (K x ::: xs) = K x ::: if unbox p x then never else delay (run (adv xs))
    run (Fun f ::: xs) = Fun (box (\t -> let (a :* b) = unbox f t in (a :* (unbox p a || b)))) ::: delay (run (adv xs))

stopWith :: Box (a -> Maybe' a) -> Beh a -> Beh a
stopWith p (Beh b) = Beh (run b)
  where
    run (K x ::: xs) =
      case unbox p x of
        Just' a -> K a ::: never
        Nothing' -> K x ::: delay (run (adv xs))
    run (Fun f ::: xs) =
      Fun
        ( box
            ( \t ->
                let (a :* b) = unbox f t
                 in case unbox p a of
                      Just' a' -> a' :* True
                      Nothing' -> a :* b
            )
        )
        ::: delay (run (adv xs))

instance (Continuous a) => Continuous (Beh a) where
  progressInternal inp (Beh (x ::: xs@(Delay cl _))) =
    if inputInClock inp cl
      then Beh (adv' xs inp)
      else progressInternal inp (Beh (x ::: xs))
  progressAndNext inp (Beh (x ::: xs@(Delay cl _))) =
    if inputInClock inp cl
      then let n = adv' xs inp in (Beh n, nextProgress n)
      else let (n, cl') = progressAndNext inp x in (Beh (n ::: xs), cl `clockUnion` cl')
  nextProgress (Beh (x ::: (Delay cl _))) = nextProgress x `clockUnion` cl