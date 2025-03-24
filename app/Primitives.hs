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
  Fun :: !(Box (Time -> (a :* Bool))) -> Fun a

continuous ''Fun

apply :: Fun a -> (Time -> a)
apply (K a) = Prelude.const a
apply (Fun f) = \t -> let (a :* _) = unbox f t in a

mapF :: Box (a -> b) -> Fun a -> Fun b
mapF f (K a) = K (unbox f a)
mapF f (Fun f') = Fun (box (\t -> let (a :* b) = unbox f' t in (unbox f a :* b)))

mapFBool :: Box (a -> Bool) -> Fun a -> Fun a
mapFBool _ (K a) = K a
mapFBool g (Fun f') = Fun (box (\t -> let (a :* _) = unbox f' t in (a :* unbox g a)))

delayCF :: O(a -> C b) -> O(a -> b)
delayCF (Delay c f) = Delay c (\inp a -> advC' (f inp a) inp)