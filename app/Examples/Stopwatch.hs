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

timerExample :: C VStack'
timerExample = do
  startElapsedTime <- elapsedTime

  startBtn <- mkButton' (const (K ("Start" :: Text)))
  let startEv = btnOnClick startBtn
  stopBtn <- mkButton' (const (K ("Stop" :: Text)))
  let stopEv = btnOnClick stopBtn

  let startTime = Event.triggerAwait (box (\_ a -> a)) startEv startElapsedTime
  let startTime' = Event.stepper 0 startTime
  timeLabName <- mkLabel' (const (K ("Current Time:" :: Text)))
  swLabName <- mkLabel' (const (K ("Elapsed Time:" :: Text)))

  let stopTime = Event.triggerAwait (box (\_ a -> a)) stopEv startElapsedTime
  let stopTime' = Event.triggerAwait (box (\a b -> a-b)) stopTime startTime'
  let input = Event.stepper 0 (Event.interleave (box (\x _ -> x)) startTime stopTime')
  --let input' = Behaviour.map (box (\(start :* running) -> start)) input
  timeLab <- mkLabel' startElapsedTime
  stopWatchLab <- mkLabel' input
  time <- mkConstHStack' (timeLabName :* timeLab)
  sw <- mkConstHStack' (swLabName :* stopWatchLab)
  buttons <- mkConstHStack' (startBtn :* stopBtn)
  mkConstVStack' (time :* sw :* buttons)

main :: IO ()
main = runApplication' timerExample