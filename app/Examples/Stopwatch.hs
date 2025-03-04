{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Behaviour
import Event
import Primitives
import WidgetRattus
import WidgetRattus.Signal (Sig ((:::)))
import Widgets
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

elapsedTime' :: C (NominalDiffTime -> Beh NominalDiffTime)
elapsedTime' =
  do
    startTime <- time
    return (\f -> Beh (Fun (box (\currentTime -> (f + diffTime currentTime startTime) :* False)) ::: never))

timerExample :: C VStack'
timerExample = do
  startElapsedTime <- elapsedTime

  startBtn <- mkButton' (mkConstText "Start")
  let startEv = btnOnClick startBtn
  stopBtn <- mkButton' (mkConstText "Stop")
  let stopEv = btnOnClick stopBtn

  let startTime :: Ev (NominalDiffTime -> Beh NominalDiffTime) =
        mkEv' (box (delay (let _ = adv (unbox startEv) in elapsedTime')))
  let stopTime :: Ev (NominalDiffTime -> Beh NominalDiffTime) =
        mkEv (box (delay (let _ = adv (unbox stopEv) in const . K)))

  timeLabName <- mkLabel' (mkConstText "Current Time:")
  swLabName <- mkLabel' (mkConstText "Elapsed Time:")

  let input = Event.interleave (box (\x _ -> x)) startTime stopTime
  let stopWatchSig :: Beh NominalDiffTime =
        switchR (constK 0) input

  timeLab <- mkLabel' startElapsedTime
  stopWatchLab <- mkLabel' stopWatchSig
  time <- mkConstHStack' (timeLabName :* timeLab)
  sw <- mkConstHStack' (swLabName :* stopWatchLab)
  buttons <- mkConstHStack' (startBtn :* stopBtn)
  mkConstVStack' (time :* sw :* buttons)

main :: IO ()
main = runApplication' timerExample