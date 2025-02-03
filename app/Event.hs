{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
module Event where

import WidgetRattus
import WidgetRattus.Signal

data Ev a = Ev !(O (Sig (a)))