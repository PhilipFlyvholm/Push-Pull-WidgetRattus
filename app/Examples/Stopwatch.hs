{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Behaviour
import Data.Text
import Event
import Primitives
import WidgetRattus
import WidgetRattus.Signal (Sig ((:::)))
import Widgets
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

elapsedTime' :: C (Fun Time NominalDiffTime -> Beh NominalDiffTime)
elapsedTime' =
  do
    startTime <- time
    return (\f -> Beh (Fun (box (\currentTime -> (apply f startTime + diffTime currentTime startTime) :* False)) ::: never))

cacheFunTime :: C (Fun Time NominalDiffTime -> Beh NominalDiffTime)
cacheFunTime =
  do
    t <- time
    return (\f -> const (K (apply f t)))

timerExample :: C VStack'
timerExample = do
  startElapsedTime <- elapsedTime

  startBtn <- mkButton' (const (K ("Start" :: Text)))
  let startEv = btnOnClick startBtn
  stopBtn <- mkButton' (const (K ("Stop" :: Text)))
  let stopEv = btnOnClick stopBtn

  let startTime :: Ev (Fun Time NominalDiffTime -> Beh NominalDiffTime) =
        mkEv' (box (delay (let _ = adv (unbox startEv) in elapsedTime')))
  let stopTime :: Ev (Fun Time NominalDiffTime -> Beh NominalDiffTime) = 
        mkEv' (box (delay (let _ = adv (unbox stopEv) in cacheFunTime)))

  timeLabName <- mkLabel' (const (K ("Current Time:" :: Text)))
  swLabName <- mkLabel' (const (K ("Elapsed Time:" :: Text)))

  let input = Event.interleave (box (\x _ -> x)) startTime stopTime
  let stopWatchSig :: Beh NominalDiffTime =
        switchR (const (K 0)) input

  timeLab <- mkLabel' startElapsedTime
  stopWatchLab <- mkLabel' stopWatchSig
  time <- mkConstHStack' (timeLabName :* timeLab)
  sw <- mkConstHStack' (swLabName :* stopWatchLab)
  buttons <- mkConstHStack' (startBtn :* stopBtn)
  mkConstVStack' (time :* sw :* buttons)

main :: IO ()
main = runApplication' timerExample