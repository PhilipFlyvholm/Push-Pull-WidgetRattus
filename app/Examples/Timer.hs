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

counterAndTimer :: C VStack'
counterAndTimer = do
  startElapsedTime <- elapsedTime

  maxSlider <- mkSlider' 50 (const (K 1)) (const (K 100))
  resetBtn <- mkButton' (const (K ("Reset timer" :: Text)))
  let lastReset = stepper 0 $ triggerAwait (box (\_ n -> n)) (btnOnClick resetBtn) startElapsedTime

  let maxSig = sldCurr maxSlider
  let timeSinceLastReset = Behaviour.zipWith (box (-)) startElapsedTime lastReset
  let currentTimer = Behaviour.zipWith (box (Prelude.min . round . toRational)) timeSinceLastReset maxSig

  maxText <- mkLabel' (Behaviour.map (box (\n -> "Max: " <>  toText n)) maxSig)
  text <- mkLabel' (Behaviour.map (box (\n -> "Current: " <>  toText n)) currentTimer)
  pb <- mkProgressBar' (const (K 0)) maxSig currentTimer
  mkConstVStack' (maxText :* maxSlider :* text :* resetBtn :* pb)

main :: IO ()
main = runApplication' counterAndTimer