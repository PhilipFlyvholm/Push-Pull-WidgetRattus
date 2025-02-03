{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}

module Primitives where

import WidgetRattus

data Fun t a = K !a | Fun !(Box (t -> a))

apply :: Fun t a -> (t -> a)
apply (K a) = Prelude.const a
apply (Fun f) = unbox f

mapF :: Box (a -> b) -> Fun t a -> Fun t b
mapF f (K a) = K (unbox f a)
mapF f (Fun t) = Fun (box (unbox f . unbox t))