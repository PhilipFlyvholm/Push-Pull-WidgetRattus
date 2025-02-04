{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Examples.Test (window, window2) where

import Behaviour
import Data.Text hiding (all, filter, map)
import WidgetRattus
import WidgetRattus.Signal
import WidgetRattus.Widgets
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

everySecondSig :: O (Sig ())
everySecondSig = mkSig (box (WidgetRattus.timer 1000000))

window :: C VStack
window = do
  t <- Behaviour.timer
  -- t' <- discretize t
  let tSig = Nothing' ::: Behaviour.triggerAwait (box (\_ d -> d)) everySecondSig t

  text <- mkLabel $ WidgetRattus.Signal.map (box (\(Just' d') -> d')) tSig
  btn <- mkButton (const ("Increment" :: Text))
  let sig = btnOnClickSig btn
  let sig' = scanAwait (box (\n _ -> n + 1 :: Int)) 0 sig
  lbl <- mkLabel sig'
  mkConstVStack (text :* lbl :* btn)

window2 :: C VStack
window2 = do
  t <- Behaviour.timer
  t' <- discretize t
  let tSig = WidgetRattus.Signal.triggerAwait (box (\_ d -> d)) 0 everySecondSig t'

  text <- mkLabel tSig
  btn <- mkButton (const ("Increment" :: Text))
  let sig = btnOnClickSig btn
  let sig' = scanAwait (box (\n _ -> n + 1 :: Int)) 0 sig
  lbl <- mkLabel sig'
  mkConstVStack (text :* lbl :* btn)
