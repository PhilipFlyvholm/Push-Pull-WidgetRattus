{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}

module Widgets where

import Behaviour
import Event
import WidgetRattus
import WidgetRattus.Widgets
import qualified WidgetRattus.Widgets.InternalTypes as WR
import Primitives

class IsWidget' a where
  mkOldWidget :: a -> C Widget

class Widgets' ws where
      toWidgetList' :: ws -> C (List Widget)

instance {-# OVERLAPPABLE #-} IsWidget' w => Widgets' w where
      toWidgetList' w = do
        w' <- mkOldWidget w
        return [ w' ]

instance {-# OVERLAPPING #-} (Widgets' w, Widgets' v) => Widgets' (w :* v) where
      toWidgetList' (w :* v) = do
        left <- toWidgetList' w
        right <- toWidgetList' v
        return $ left +++ right


instance {-# OVERLAPPING #-} (Widgets' w) => Widgets' (List w) where
      toWidgetList' w = do
        toWidgetList' w

mkButton' :: (Displayable a) => Beh a -> C Button'
mkButton' t = do
   c <- chan
   return Button' {btnLabel = t, onClickChan = c}

mkVStack' :: IsWidget a => Beh(List a) -> C VStack'
mkVStack' wl = do
      return (VStack' wl)

mkConstVStack' :: Widgets' ws => ws -> C VStack'
mkConstVStack' w = do
  ws <- toWidgetList' w
  mkVStack' $ Behaviour.const (K ws)

data Button' where
  Button' :: (Displayable a) => {onClickChan :: !(Chan ()), btnLabel :: !(Beh a)} -> Button'

data VStack' where
  VStack' :: (IsWidget a) => !(Beh (List a)) -> VStack'

instance {-# OVERLAPPABLE #-} (IsWidget a) => IsWidget' a where
  mkOldWidget a = do
    let w = mkWidget a
    return w

instance IsWidget' Button' where
  mkOldWidget Button' {onClickChan = click, btnLabel = b} = do
    w <- discretize b
    return $ WR.mkWidget (WR.Button {btnContent = w, btnClick = click})

instance IsWidget' VStack' where
  mkOldWidget (VStack' ws) = do
    ws' <- discretize ws
    let d = WR.VStack ws'
    return $ WR.mkWidget d


btnOnClick :: Button' -> Ev ()
btnOnClick b =
  let ch = onClickChan b
   in mkEv (box (wait ch))

runApplication' :: IsWidget' a => C a -> IO()
runApplication' w =
  runApplication ( do
        w' <- w
        mkOldWidget w'
    )
