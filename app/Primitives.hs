{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Primitives where

import WidgetRattus
import WidgetRattus.InternalPrimitives

data FunOld t a = KOld !a | FunOld !(Box (t -> (a :* Bool)))

data Fun a where
  K :: !a -> Fun a
  Fun :: Stable s => !s -> !(Box (Time -> (a :* Bool :* s))) -> Fun a

continuous ''Fun
apply :: Fun a -> (Time -> a)
apply (K a) = Prelude.const a
apply (Fun _ f) = \t -> let (a :* _) = unbox f t in a

mapF :: Box (a -> b) -> Fun a -> Fun b
mapF f (K a) = K (unbox f a)
mapF f (Fun s f') = Fun s (box (\t -> let (a :* b :* s'') = unbox f' t in (unbox f a :* b :* s'')))

mapFBool :: Box (a -> Bool) -> Fun a -> Fun a
mapFBool _ (K a) = K a
mapFBool g (Fun s f') = Fun s (box (\t -> let (a :* _ :* s'') = unbox f' t in (a :* unbox g a :* s'')))

delayCF :: O(a -> C b) -> O(a -> b)
delayCF (Delay c f) = Delay c (\inp a -> advC' (f inp a) inp)