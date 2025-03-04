{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Behaviour
import Event
import WidgetRattus
import Widgets
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import Data.Text
import Primitives

counterAndTimer :: C VStack'
counterAndTimer = do
  counterBtn <- mkButton' (const (K ("Increment" :: Text)))
  let counterEv = scan (box (\n _ -> n + 1 :: Int)) 0 $ btnOnClickEv counterBtn

  lbl <- mkLabel' $ stepper 0 counterEv
  mkConstVStack' (lbl :* counterBtn)

main :: IO ()
main = runApplication' counterAndTimer