{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
module Event where

import WidgetRattus
import WidgetRattus.Signal

newtype Ev a = Ev (O (Sig a))