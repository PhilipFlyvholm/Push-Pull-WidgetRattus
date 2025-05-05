{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Primitives where

import WidgetRattus
import WidgetRattus.InternalPrimitives

data Fun a where
  K :: !a -> Fun a
  Fun :: (Stable s) => !s -> !(Box (s -> Time -> (a :* Maybe' s))) -> Fun a

continuous ''Fun

apply :: Fun a -> (Time -> a)
apply (K a) = Prelude.const a
apply (Fun s f) = \t -> let (a :* _) = unbox f s t in a

mapF :: Box (a -> b) -> Fun a -> Fun b
mapF f (K a) = K (unbox f a)
mapF f (Fun s f') = Fun s (box (\s t -> let (a :* s') = unbox f' s t in (unbox f a :* s')))

delayCF :: O(a -> C b) -> O(a -> b)
delayCF (Delay c f) = Delay c (\inp a -> advC' (f inp a) inp)
