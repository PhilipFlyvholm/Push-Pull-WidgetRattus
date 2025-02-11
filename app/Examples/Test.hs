{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Examples.Test (window) where

import Behaviour
import Event
import WidgetRattus
import WidgetRattus.Widgets
  ( VStack,
    mkConstVStack,
    mkLabel,
  )
import Widgets (btnOnClick, mkButton)
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

window :: C VStack
window = do
  elapsedTime' <- Behaviour.elapsedTime
  tSig <- discretize elapsedTime'

  text <- mkLabel tSig
  btn <- mkButton timeBehaviour
  let ev = btnOnClick btn
  -- let ev' = Event.scan (box (\n _ -> n + 1 :: Int)) 0 ev
  -- sig <- discretize (Event.stepper ev' 0)
  beh <- discretize $ Event.scanB (box (\n _ -> n + 1 :: Int)) 0 ev
  lbl <- mkLabel beh
  mkConstVStack (text :* lbl :* btn)
