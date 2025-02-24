{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Behaviour
import Event
import WidgetRattus
import Widgets
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import Data.Text
import Primitives

roundNominalDiffTime :: NominalDiffTime -> Integer
roundNominalDiffTime = round . toRational

counterAndTimer :: C VStack'
counterAndTimer = do
  startElapsedTime <- elapsedTime

  resetBtn <- mkButton' (const (K ("reset timer" :: Text)))
  let lastReset = stepper 0 $ triggerAwait (box (\_ n -> n)) (btnOnClick resetBtn) startElapsedTime

  let currentTimer = Behaviour.zipWith (box (-)) startElapsedTime lastReset

  text <- mkLabel' (Behaviour.map (box (toText . round . toRational)) currentTimer)

  counterBtn <- mkButton' timeBehaviour
  let counterEv = scan (box (\n _ -> n + 1 :: Int)) 0 $ btnOnClick counterBtn

  lbl <- mkLabel' $ stepper 0 counterEv
  mkConstVStack' (text :* lbl :* counterBtn :* resetBtn)

main :: IO ()
main = runApplication' counterAndTimer