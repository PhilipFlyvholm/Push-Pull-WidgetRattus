{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Behaviour
import Event
import WidgetRattus
import Widgets
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import Data.Text
import Primitives


timerExample :: C VStack'
timerExample = do
  startElapsedTime <- elapsedTime

  maxSlider <- mkSlider' 50 (const (K 1)) (const (K 100))
  resetBtn <- mkButton' (const (K ("Reset timer" :: Text)))
  let trig = Event.triggerAwait (box (\_ y -> y)) (btnOnClick resetBtn) startElapsedTime
  let resetEv = Event.filter (box (>= 5)) trig
  let lastReset = stepper 0 $ triggerAwait (box (\_ n -> n)) resetEv startElapsedTime

  let maxSig = sldCurr maxSlider
  let timeSinceLastReset = Behaviour.zipWith (box (-)) startElapsedTime lastReset
  let currentTimer = Behaviour.zipWith (box (\a b -> round (toRational a) :* b)) timeSinceLastReset maxSig
  let stoppedCurrentTimer = Behaviour.stop (box (\(a :* b) -> a >= b)) currentTimer
  let d = Event.scan (box (\b a -> b)) (currentTimer) resetEv

  let stoppedCurrentTimerRecursive = Event.switchR stoppedCurrentTimer d
  let displayTimer = Behaviour.map (box (\(cur :* _) -> cur)) stoppedCurrentTimerRecursive
  maxText <- mkLabel' (Behaviour.map (box (\n -> "Max: " <>  toText n)) maxSig)
  text <- mkLabel' (Behaviour.map (box (\n -> "Current: " <>  toText n)) displayTimer)
  text' <- mkLabel' (Behaviour.map (box (\n -> "Current (raw): " <>  toText n)) currentTimer)
  pb <- mkProgressBar' (const (K 0)) maxSig displayTimer
  mkConstVStack' (maxText :* maxSlider :* text :* text' :* resetBtn :* pb)

main :: IO ()
main = runApplication' timerExample