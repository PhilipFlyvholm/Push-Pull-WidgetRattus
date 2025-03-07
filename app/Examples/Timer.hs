{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Behaviour
import Event
import WidgetRattus
import Widgets
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import Primitives

nominalToInt :: (Integral b, Real a) => a -> b
nominalToInt x = round $ toRational x

timeFromDiff :: NominalDiffTime -> Time
timeFromDiff dt = addTime dt (Time (toEnum 0) 0)

timeFromLastTimer :: NominalDiffTime -> Int -> Beh (NominalDiffTime :* Int)
timeFromLastTimer timeAtResetEvent max = const (Fun (box (\t -> (diffTime t (timeFromDiff timeAtResetEvent) :* max) :* False)))

createStop :: NominalDiffTime -> (NominalDiffTime :* Int -> Beh (NominalDiffTime :* Int))
createStop timeAtResetEvent (_ :* max) = stop (box (\(currentTime :* max') -> nominalToInt currentTime >= max')) (timeFromLastTimer timeAtResetEvent max)

timerExample :: C VStack'
timerExample = do
  let startElapsedTime = timeBehaviour


  maxSlider <- mkSlider' 50 (constK 1) (constK 100)
  let maxBeh = sldCurr maxSlider

  resetBtn <- mkButton' $ mkConstText "Reset timer"

  let resetEv :: Ev ((NominalDiffTime :* Int) -> Beh (NominalDiffTime :* Int))
        = Event.triggerAwait (box (\ _ timeAtResetEvent -> createStop timeAtResetEvent)) (btnOnClickEv resetBtn) (Behaviour.map (box(\t -> diffTime t (Time (toEnum 0) 0))) startElapsedTime)

  now <- time
  let timer = switchR (createStop (diffTime now (Time (toEnum 0) 0)) (0 :* 5)) resetEv 

  maxText <- mkLabel' (Behaviour.map (box (\n -> "Max: " <> toText n)) maxBeh)
  text <- mkLabel' (Behaviour.map (box (\(t :* _) -> "Current: " <> toText (nominalToInt t))) timer)
  pb <- mkProgressBar' (constK 0) maxBeh (Behaviour.map (box(\(curr :* _) -> nominalToInt curr)) timer)
  mkConstVStack' $ maxText :* maxSlider :* text :* resetBtn :* pb

main :: IO ()
main = runApplication' timerExample