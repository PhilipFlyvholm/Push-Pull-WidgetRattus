{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Primitives where

import WidgetRattus

data Fun a = K !a | Fun !(Box (Time -> a))

continuous ''Fun

apply :: Fun a -> (Time -> a)
apply (K a) = Prelude.const a
apply (Fun f) = unbox f

mapF :: Box (a -> b) -> Fun a -> Fun b
mapF f (K a) = K (unbox f a)
mapF f (Fun t) = Fun (box (unbox f . unbox t))