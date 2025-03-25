{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Primitives where

import WidgetRattus
import WidgetRattus.InternalPrimitives

data Fun s a where
  K :: !a -> Fun s a
  Fun :: (Stable s) => !s -> !(Box (s -> Time -> (a :* Bool :* s))) -> Fun s a

continuous ''Fun

apply :: Fun s a -> (Time -> a)
apply (K a) = Prelude.const a
apply (Fun s f) = \t -> let (a :* _) = unbox f s t in a

mapF :: Box (a -> b) -> Fun s a -> Fun s b
mapF f (K a) = K (unbox f a)
mapF f (Fun s f') = Fun s (box (\s t -> let (a :* b :* s') = unbox f' s t in (unbox f a :* b :* s')))

mapFBool :: Box (a -> Bool) -> Fun s a -> Fun s a
mapFBool _ (K a) = K a
mapFBool g (Fun s f') = Fun s (box (\s t -> let (a :* _ :* s') = unbox f' s t in (a :* unbox g a :* s')))

delayCF :: O(a -> C b) -> O(a -> b)
delayCF (Delay c f) = Delay c (\inp a -> advC' (f inp a) inp)