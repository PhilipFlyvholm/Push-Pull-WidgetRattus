{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Widgets where

import Behaviour
import Event
import WidgetRattus
import WidgetRattus.Widgets
import qualified WidgetRattus.Widgets.InternalTypes as WR

mkButton :: (Displayable a) => Beh a -> C WR.Button
mkButton t = do
  c <- chan
  sig <- discretize t
  return WR.Button {btnContent = sig, btnClick = c}

{-
data Button where
  Button :: (Displayable a) => {onClickChan :: !(Chan ()), btnLabel :: !(Beh a)} -> Widgets.Button
-}
{--
instance IsWidget Widgets.Button where
  mkWidgetNode Button {onClickChan = click, btnLabel = b} = ?
-}
btnOnClick :: WR.Button -> Ev ()
btnOnClick b =
  let ch = btnClick b
   in mkEv (box (wait ch))

-- continuous ''Button