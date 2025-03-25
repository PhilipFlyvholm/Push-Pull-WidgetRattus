{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Behaviour where

import Data.Ratio
import Primitives
import WidgetRattus
import WidgetRattus.InternalPrimitives (Continuous (..), O (Delay), adv', clockUnion, inputInClock)
import WidgetRattus.Signal hiding (const, integral, jump, map, switch)
import Prelude hiding (const, map, zipWith)

newtype Beh s a = Beh (Sig (Fun s a))

unwrap :: Beh s a -> Sig (Fun s a)
unwrap (Beh a) = a

const :: Fun s a -> Beh s a
const x = Beh (x ::: never)

constK :: a -> Beh s a
constK x = Beh (K x ::: never)

timeBehaviour :: Beh () Time
timeBehaviour = const (Fun () (box (\s t -> t :* False :* s)))

map :: Box (a -> b) -> Beh s a -> Beh s b
map f (Beh (x ::: xs)) = Beh (mapF f x ::: delay (unwrap $ Behaviour.map f (Beh (adv xs))))

sampleInterval :: O ()
sampleInterval = timer 20000

discretize :: (Stable s) => Beh s a -> C (Sig a)
discretize (Beh (K x ::: xs)) = do
  let rest = delayC $ delay (let x' = adv xs in discretize (Beh x'))
  return $ x ::: rest
discretize (Beh (Fun s f ::: xs)) = do
  t <- time
  let (cur :* b :* s') = unbox f s t

  let rest =
        if b
          then delayC $ delay (let sig = adv xs in discretize (Beh sig))
          else
            delayC $
              delay
                ( case select xs sampleInterval of
                    Fst x _ -> discretize (Beh x)
                    Snd beh' _ -> discretize (Beh (Fun s' f ::: beh'))
                    Both x _ -> discretize (Beh x)
                )
  return (cur ::: rest)

elapsedTime :: C (Beh () NominalDiffTime)
elapsedTime = do
  startTime <- time
  return $ Beh (Fun () (box (\s currentTime -> diffTime currentTime startTime :* False :* s)) ::: never)

withTime :: O (Time -> a) -> O a
withTime delayed =
  delayC $ delay (let f = adv delayed in do f <$> time)

switch :: Beh s a -> O (Beh s a) -> Beh s a
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
zipWith :: (Stable a, Stable b, Stable s) => Box (a -> b -> c) -> Beh s a -> Beh s b -> Beh s c
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
    app (Fun xs x') (Fun _ y') =
      Fun
        xs
        ( box
            ( \s t ->
                let (a :* ab :* xs') = unbox x' s t
                    (b :* bb :* _) = unbox y' s t
                    left = unbox f a b
                 in left :* (ab || bb) :* xs'
            )
        )
    app (Fun xs x') (K y') =
      Fun
        xs
        ( box
            ( \s t ->
                let (a :* ab :* xs') = unbox x' s t
                    left = unbox f a y'
                 in left :* ab :* xs'
            )
        )
    app (K x') (Fun ys y') =
      Fun
        ys
        ( box
            ( \s t ->
                let (b :* bb :* ys') = unbox y' s t
                    left = unbox f x' b
                 in left :* bb :* ys'
            )
        )

-- | Variant of 'zipWith' with three behaviours.
zipWith3 :: forall a b c d s. (Stable a, Stable b, Stable c, Stable s) => Box (a -> b -> c -> d) -> Beh s a -> Beh s b -> Beh s c -> Beh s d
zipWith3 f as bs cs = Behaviour.zipWith (box (\f' x -> unbox f' x)) cds cs
  where
    cds :: Beh s (Box (c -> d))
    cds = Behaviour.zipWith (box (\a b -> box (\c -> unbox f a b c))) as bs

stop :: (Stable s) => Box (a -> Bool) -> Beh s a -> Beh s a
stop p (Beh b) = Beh (run b)
  where
    run (K x ::: xs) = K x ::: if unbox p x then never else delay (run (adv xs))
    run (Fun s f ::: xs) = Fun s (box (\s' t -> let (a :* b :* s'') = unbox f s' t in (a :* (unbox p a || b) :* s''))) ::: delay (run (adv xs))

stopWith :: (Stable s) => Box (a -> Maybe' a) -> Beh s a -> Beh s a
stopWith p (Beh b) = Beh (run b)
  where
    run (K x ::: xs) =
      case unbox p x of
        Just' a -> K a ::: never
        Nothing' -> K x ::: delay (run (adv xs))
    run (Fun s f ::: xs) =
      Fun
        s
        ( box
            ( \s' t ->
                let (a :* b :* s'') = unbox f s' t
                 in case unbox p a of
                      Just' a' -> a' :* True :* s''
                      Nothing' -> a :* b :* s''
            )
        )
        ::: delay (run (adv xs))

dt :: Int
dt = 20000

dtf :: Float
dtf = fromRational (fromIntegral dt % 1000000)

-- integral :: Float -> Beh Float -> C (Beh Float)
-- integral cur (Beh (K x ::: xs)) = do
--   t <- time
--   let rest =
--         delayC $
--           delay
--             ( do
--                 t' <- time
--                 let tDiff = diffTime t' t
--                 let r = cur + x * fromRational (toRational tDiff)
--                 let result = integral r (Beh (adv xs))
--                 unwrap <$> result
--             )
--   let curF =
--         Fun
--           ( box
--               ( \t' ->
--                   let tDiff = diffTime t' t
--                    in cur + x * fromRational (toRational tDiff) :* False :* () -- TODO: Use state
--               )
--           )
--   return $ Beh (curF ::: rest)
-- integral cur (Beh (x ::: xs)) = do
--   let rest =
--         delayC $
--           delay
--             ( let d = select xs (timer dt)
--                in do
--                     t <- time
--                     let result =
--                           ( case d of
--                               Fst xs' _ -> integral cur (Beh xs')
--                               Snd xs' _ -> integral (cur + apply x t * dtf) (Beh (x ::: xs'))
--                               Both (x' ::: xs') _ -> integral (cur + apply x' t * dtf) (Beh (x' ::: xs'))
--                           )
--                     unwrap <$> result
--             )

--   return $ Beh (K cur ::: rest)

-- derivative :: Beh Float -> C (Beh Float)
-- derivative (Beh (x ::: xs)) = do
--   t <- time
--   Beh <$> der 0 (apply x t) (x ::: xs)
--   where
--     der :: Float -> Float -> Sig (Fun Float) -> C (Sig (Fun Float))
--     der 0 _ (x ::: xs) =
--       do
--         t <- time
--         return
--           ( K 0
--               ::: delayC
--                 ( delay
--                     ( let x' ::: xs' = adv xs
--                        in do
--                             t' <- time
--                             der ((apply x' t' - apply x t) / dtf) (apply x t) (x' ::: xs')
--                     )
--                 )
--           )
--     der d last (x ::: xs) =
--       do
--         t <- time
--         return
--           ( K d
--               ::: delayC
--                 ( delay
--                     ( let ticker = select xs (timer dt)
--                        in do
--                             t' <- time
--                             case ticker of
--                               Fst xs' _ -> der d last xs'
--                               Snd xs' _ -> der ((apply x t - last) / dtf) (apply x t) (x ::: xs')
--                               Both (x' ::: xs') _ -> der ((apply x' t' - last) / dtf) (apply x' t') (x' ::: xs')
--                     )
--                 )
--           )

intergral' :: (Stable s) => Float -> s -> Beh s Float -> C (Beh (Float :* Time :* s) Float)
intergral' cur s (Beh (x ::: xs)) = do
  t <- time
  let rest =
        delayC
          ( delay
              ( do
                  t' <- time
                  let tDiff = diffTime t' t
                  let dt = fromRational (toRational tDiff)
                  let (v :* s') =
                        case x of
                          K a -> a :* s
                          Fun s' f -> let (v :* _ :* s'') = unbox f s' t' in (v :* s'')
                  unwrap <$> intergral' (cur + v * dt) s' (Beh (adv xs))
              )
          )
  let curF =
        Fun
          (cur :* t :* s)
          ( box
              ( \(lv :* lt :* ls) t' ->
                  let tDiff = diffTime t' lt
                      dt = fromRational (toRational tDiff)
                      (v :* s') =
                        case x of
                          K a -> a :* ls
                          Fun s' f -> let (v :* _ :* s'') = unbox f s' t' in (v :* s'')
                   in lv + v * dt :* False :* (lv+v*dt :* t' :* s')
              )
          )
  return $ Beh (curF ::: rest)

derivative' :: (Stable s) => Beh s Float -> C (Beh () Float)
derivative' (Beh (x ::: xs)) = do
  t <- time
  Beh <$> der (apply x t) (x ::: xs)
  where
    der :: (Stable s) => Float -> Sig (Fun s Float) -> C (Sig (Fun () Float))
    der last (x ::: xs) = do
      t <- time
      let curF =
            Fun () $
              box
                ( \_ t' ->
                    let tDiff = diffTime t' t
                        dt = fromRational (toRational tDiff)
                     in (apply x t - last) / dt :* False :* () -- TODO: Use state
                )
      let rest =
            delayC
              ( delay
                  ( do
                      t' <- time
                      der (apply x t') (adv xs)
                  )
              )
      return (curF ::: rest)

instance (Continuous a, Stable s) => Continuous (Beh s a) where
  progressInternal inp (Beh (x ::: xs@(Delay cl _))) =
    if inputInClock inp cl
      then Beh (adv' xs inp)
      else progressInternal inp (Beh (x ::: xs))
  progressAndNext inp (Beh (x ::: xs@(Delay cl _))) =
    if inputInClock inp cl
      then let n = adv' xs inp in (Beh n, nextProgress n)
      else let (n, cl') = progressAndNext inp x in (Beh (n ::: xs), cl `clockUnion` cl')
  nextProgress (Beh (x ::: (Delay cl _))) = nextProgress x `clockUnion` cl

-- Prevent functions from being inlined too early for the rewrite
-- rules to fire.

{-# NOINLINE [1] map #-}

{-# NOINLINE [1] const #-}

{-# NOINLINE [1] constK #-}

{-# NOINLINE [1] switch #-}

{-# RULES
"beh.map/beh.map" forall f g xs.
  map f (map g xs) =
    map (box (unbox f . unbox g)) xs
"beh.constK/beh.map" forall (f :: (Stable b) => Box (a -> b)) x.
  map f (constK x) =
    let x' = unbox f x in constK x'
"beh.const/beh.switch" forall x xs.
  switch (const x) xs =
    Beh (x ::: delay (unwrap (adv xs)))
"beh.constK/beh.switch" forall x xs.
  switch (constK x) xs =
    Beh (K x ::: delay (unwrap (adv xs)))
  #-}
