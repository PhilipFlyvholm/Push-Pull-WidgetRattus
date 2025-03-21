{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Primitives where

import WidgetRattus
import WidgetRattus.InternalPrimitives

data Fun t a = K !a | Fun !(Box (t -> (a :* Bool)))

continuous ''Fun

apply :: Fun t a -> (t -> a)
apply (K a) = Prelude.const a
apply (Fun f) = \t -> let (a :* _) = unbox f t in a

mapF :: Box (a -> b) -> Fun t a -> Fun t b
mapF f (K a) = K (unbox f a)
mapF f (Fun f') = Fun (box (\t -> let (a :* b) = unbox f' t in (unbox f a :* b)))

mapFBool :: Box (a -> Bool) -> Fun t a -> Fun t a
mapFBool _ (K a) = K a
mapFBool g (Fun f') = Fun (box (\t -> let (a :* _) = unbox f' t in (a :* unbox g a)))

delayCF :: O(a -> C b) -> O(a -> b)
delayCF (Delay c f) = Delay c (\inp a -> advC' (f inp a) inp)