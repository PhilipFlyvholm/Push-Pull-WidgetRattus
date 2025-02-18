-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE GADTs #-}
-- {-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# LANGUAGE ConstrainedClassMethods #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE OverloadedLists #-}
-- {-# LANGUAGE TypeFamilies #-}
module Widgets where

-- import Behaviour
-- import Event
-- import WidgetRattus
-- import WidgetRattus.Widgets hiding (btnClick, popChild, popEvent, popCurr)
-- import qualified WidgetRattus.Widgets.InternalTypes as WR
-- import Primitives
-- import Data.Text
-- import WidgetRattus.Widgets.InternalTypes (HStack(HStack))
-- import WidgetRattus.Signal (Sig ((:::)), map)
-- import Prelude hiding (max, min)

-- class IsWidget' a where
--   mkOldWidget :: a -> C Widget

-- class Widgets' ws where
--       toWidgetList' :: ws -> C (List Widget)

-- instance {-# OVERLAPPABLE #-} IsWidget' w => Widgets' w where
--       toWidgetList' w = do
--         w' <- mkOldWidget w
--         return [ w' ]

-- instance {-# OVERLAPPING #-} (Widgets' w, Widgets' v) => Widgets' (w :* v) where
--       toWidgetList' (w :* v) = do
--         left <- toWidgetList' w
--         right <- toWidgetList' v
--         return $ left +++ right


-- instance {-# OVERLAPPING #-} (Widgets' w) => Widgets' (List w) where
--       toWidgetList' w = do
--         toWidgetList' w

-- instance {-# OVERLAPPABLE #-} (IsWidget a) => IsWidget' a where
--   mkOldWidget a = do
--     let w = mkWidget a
--     return w

-- -- HStack 
-- data HStack' where
--       HStack' :: IsWidget a => !(Beh (List a)) -> HStack'

-- instance IsWidget' HStack' where
--       mkOldWidget (HStack' ws) = do
--         ws' <- discretize ws
--         return $ WR.mkWidget (HStack ws')

-- mkHStack' :: IsWidget a => Beh(List a) -> C HStack'
-- mkHStack' wl = do
--       return (HStack' wl)

-- mkConstHStack' :: Widgets' ws => ws -> C HStack'
-- mkConstHStack' w = do
--   ws <- toWidgetList' w
--   mkHStack' $ Behaviour.const (K ws)

-- -- VStack
-- data VStack' where
--   VStack' :: (IsWidget a) => !(Beh (List a)) -> VStack'

-- instance IsWidget' VStack' where
--   mkOldWidget (VStack' ws) = do
--     ws' <- discretize ws
--     return $ WR.mkWidget (WR.VStack ws')

-- mkVStack' :: IsWidget a => Beh(List a) -> C VStack'
-- mkVStack' wl = do
--       return (VStack' wl)

-- mkConstVStack' :: Widgets' ws => ws -> C VStack'
-- mkConstVStack' w = do
--   ws <- toWidgetList' w
--   mkVStack' $ Behaviour.const (K ws)


-- -- TextDropDown
-- data TextDropdown' =
--   TextDropdown' {tddCurr :: !(Beh Text), tddEvent :: !(Chan Text), tddList :: !(Beh (List Text))}

-- instance IsWidget' TextDropdown' where
--   mkOldWidget (TextDropdown' cur ev list) = do
--     cur' <- discretize cur
--     list' <- discretize list
--     return $ WR.mkWidget (WR.TextDropdown cur' ev list')

-- mkTextDropdown' :: Beh (List Text) -> Text -> C TextDropdown'
-- mkTextDropdown' opts initial = do
--   c <- chan
--   let (Ev d) = mkEv (box (wait c))
--   let beh = Beh $ WidgetRattus.Signal.map (box K) (initial ::: d)
--   return $ TextDropdown' beh c opts


-- -- Popup
-- data Popup' =
--   Popup' {popCurr :: !(Beh Bool), popEvent :: !(Chan Bool), popChild :: !(Beh Widget)}

-- instance IsWidget' Popup' where
--       mkOldWidget (Popup' curr ch child) = do
--         curr' <- discretize curr
--         child' <- discretize child
--         return $ WR.mkWidget (WR.Popup curr' ch child')

-- mkPopup' :: Beh Bool -> Beh Widget -> C Popup'
-- mkPopup' (Beh(b ::: bs)) w = do
--       c <- chan
--       t <- time
--       let d = mkEv (box (wait c))
--       let beh = Behaviour.zipWith (box Prelude.const) (Beh (b ::: bs)) (Event.stepper d (apply b t))
--       return Popup'{popCurr = beh, popEvent = c, popChild = w}


-- -- Slider
-- data Slider' =
--   Slider' {sldCurr :: !(Beh Int), sldEvent :: !(Chan Int), sldMin :: !(Beh Int), sldMax :: !(Beh Int)}

-- instance IsWidget' Slider' where
--   mkOldWidget (Slider' curr ev min max) = do
--     curr' <- discretize curr
--     min' <- discretize min
--     max' <- discretize max
--     return $ WR.mkWidget (WR.Slider curr' ev min' max')

-- mkSlider :: Int -> Beh Int -> Beh Int -> C Slider'
-- mkSlider start min max = do
--   c <- chan
--   let (Ev rest) = mkEv (box (wait c))
--   let curr = Beh $ WidgetRattus.Signal.map (box K) (start ::: rest)
--   return $ Slider' curr c min max


-- -- Button
-- data Button' where
--   Button' :: (Displayable a) => {btnClick :: !(Chan ()), btnContent :: !(Beh a)} -> Button'

-- instance IsWidget' Button' where
--   mkOldWidget (Button' click b) = do
--     w <- discretize b
--     return $ WR.mkWidget (WR.Button w click)

-- mkButton' :: (Displayable a) => Beh a -> C Button'
-- mkButton' t = do
--    c <- chan
--    return $ Button' c t


-- -- Label
-- data Label' where
--       Label' :: (Displayable a) => {labText :: !(Beh a)} -> Label'

-- instance IsWidget' Label' where
--   mkOldWidget (Label' t) = do
--     t' <- discretize t
--     return $ WR.mkWidget (WR.Label t')

-- mkLabel' :: (Displayable a) => Beh a -> C Label'
-- mkLabel' t = do
--   return $ Label' t


-- -- TextField
-- data TextField' = TextField' {tfContent :: !(Beh Text), tfInput :: !(Chan Text)}

-- instance IsWidget' TextField' where
--   mkOldWidget (TextField' b inp) = do
--     txt <- discretize b
--     return $ WR.mkWidget (WR.TextField txt inp)

-- mkTextField' :: Text -> C TextField'
-- mkTextField' txt = do
--   c <- chan
--   let (Ev d) = mkEv (box (wait c))
--   let beh = Beh $ WidgetRattus.Signal.map (box K) (txt ::: d)
--   return $ TextField' beh c

-- btnOnClick :: Button' -> Ev ()
-- btnOnClick b =
--   let ch = btnClick b
--    in mkEv (box (wait ch))

-- runApplication' :: IsWidget' a => C a -> IO()
-- runApplication' w =
--   runApplication ( do
--         w' <- w
--         mkOldWidget w'
--     )

-- data Widget' where
--   Widget' :: IsWidget' a => !a -> !(Beh Bool) -> Widget'
 

-- instance IsWidget' Widget' where
--   mkOldWidget (Widget' w beh) = do
--     beh' <- discretize beh 
--     w' <- mkOldWidget w
--     return (WR.Widget w' beh')