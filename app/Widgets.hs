{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Widgets where

import Behaviour
import Event
import qualified Monomer as M
import WidgetRattus
import WidgetRattus.Signal
import WidgetRattus.Widgets.InternalTypes
import qualified Monomer as WidgetRattus

mkButton :: (Displayable a) => Beh a -> C Button
mkButton t = do
    c <- chan
    return Button{btnLabel = t, onClickChan = c}

data AppEvent where
  AppEvent :: !(Chan a) -> !a -> AppEvent

data Button where
  Button :: (Displayable a) => {onClickChan :: !(Chan ()), btnLabel :: !(Beh a)} -> Button

instance IsWidget Button where
  mkWidgetNode Button {onClickChan = click, btnLabel = b} = 
    mkWidgetNode WidgetRattus.Widgets.Button {btnContent = discretize b, btnClick = click}

btnOnClick :: Button -> Ev ()
btnOnClick b =
  let ch = onClickChan b
   in mkEv (box (wait ch))

-- continuous ''Button