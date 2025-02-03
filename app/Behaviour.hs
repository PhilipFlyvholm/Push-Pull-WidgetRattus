{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}

module Behaviour where

import Primitives
import WidgetRattus
import WidgetRattus.Signal hiding (map)
import Prelude hiding (map)

data Beh a = Beh !(Sig (Fun Time a))

timeBehaviour :: Beh Time
timeBehaviour = Beh (Fun (box id) ::: never)

map :: Box (a -> b) -> Beh a -> Beh b
map f (Beh (x ::: xs)) = Beh (mapF f x ::: delay (let Beh (s) = map f (Beh (adv xs)) in s))