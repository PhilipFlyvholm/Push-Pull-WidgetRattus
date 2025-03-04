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

elapsedTime' :: C (NominalDiffTime -> Beh NominalDiffTime)
elapsedTime' =
  do
    startTime <- time
    return (\f -> Beh (Fun (box (\currentTime -> (f + diffTime currentTime startTime) :* False)) ::: never))

cacheFunTime :: NominalDiffTime -> Beh NominalDiffTime
cacheFunTime = const . K

timerExample :: C VStack'
timerExample = do
  startBtn <- mkButton' (const (K ("Start" :: Text)))

  mkConstVStack' (startBtn)
  -- startElapsedTime <- elapsedTime

  -- startBtn <- mkButton' (const (K ("Start" :: Text)))
  -- let startEv = btnOnClick startBtn
  -- stopBtn <- mkButton' (const (K ("Stop" :: Text)))
  -- let stopEv = btnOnClick stopBtn

  -- let startTime :: Ev (NominalDiffTime -> Beh NominalDiffTime) =
  --       mkEv' (box (delay (let _ = adv (unbox startEv) in elapsedTime')))
  -- let stopTime :: Ev (NominalDiffTime -> Beh NominalDiffTime) =
  --       mkEv (box (delay (let _ = adv (unbox stopEv) in cacheFunTime)))

  -- timeLabName <- mkLabel' (const (K ("Current Time:" :: Text)))
  -- swLabName <- mkLabel' (const (K ("Elapsed Time:" :: Text)))

  -- let input = Event.interleave (box (\x _ -> x)) startTime stopTime
  -- let stopWatchSig :: Beh NominalDiffTime =
  --       switchR (const (K 0)) input

  -- timeLab <- mkLabel' startElapsedTime
  -- stopWatchLab <- mkLabel' stopWatchSig
  -- time <- mkConstHStack' (timeLabName :* timeLab)
  -- sw <- mkConstHStack' (swLabName :* stopWatchLab)
  -- buttons <- mkConstHStack' (startBtn :* stopBtn)
  -- mkConstVStack' (time :* sw :* buttons)

main :: IO ()
main = runApplication' timerExample