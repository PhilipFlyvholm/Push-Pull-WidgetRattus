{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}

module Main where

import Behaviour
import Event
import Primitives
import WidgetRattus
import Widgets
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

nominalToInt :: NominalDiffTime -> Int
nominalToInt x = floor $ toRational x

intToNominal :: Int -> NominalDiffTime
intToNominal x = fromInteger (toInteger x)

zeroTime :: Time
zeroTime = Time (toEnum 0) 0

timeFromDiff :: NominalDiffTime -> Time
timeFromDiff dt = addTime dt zeroTime

timeFromLastTimer :: NominalDiffTime -> Int -> Beh (NominalDiffTime :* Int)
timeFromLastTimer timeAtResetEvent max = const (Fun (box (\t -> (diffTime t (timeFromDiff timeAtResetEvent) :* max) :* False)))

stopTimerPredicate :: Beh (NominalDiffTime :* Int) -> Beh (NominalDiffTime :* Int)
stopTimerPredicate = stopWith (box (\(currentTime :* max') -> if nominalToInt currentTime >= max' then Just' (intToNominal max' :* max') else Nothing'))

setResetTimer :: NominalDiffTime -> (NominalDiffTime :* Int -> Beh (NominalDiffTime :* Int))
setResetTimer timeAtResetEvent (_ :* max) = stopTimerPredicate (timeFromLastTimer timeAtResetEvent max)

setMax :: Int -> NominalDiffTime -> (NominalDiffTime :* Int -> Beh (NominalDiffTime :* Int))
setMax max timeAtResetEvent (v :* _) = stopTimerPredicate (timeFromLastTimer (timeAtResetEvent - v) max)

timerExample :: C VStack'
timerExample = do
  
  -- Initial value
  let initialMax = 50
  
  -- Time
  let startElapsedTime = timeBehaviour
  now <- time
  let initialTimer = setResetTimer (diffTime now zeroTime) (0 :* initialMax)
  
  -- Slider
  maxSlider <- mkSlider' initialMax (constK 1) (constK 100)
  let maxBeh = sldCurr maxSlider
  let maxChangeEv = sliderOnChange maxSlider
  
  let maxEv :: Ev ((NominalDiffTime :* Int) -> Beh (NominalDiffTime :* Int)) =
        Event.triggerAwait (box setMax) maxChangeEv (Behaviour.map (box (`diffTime` zeroTime)) startElapsedTime)

  -- Reset
  resetBtn <- mkButton' $ mkConstText "Reset timer"
  let resetEv :: Ev ((NominalDiffTime :* Int) -> Beh (NominalDiffTime :* Int)) =
        Event.triggerAwait (box (\_ -> setResetTimer)) (btnOnClickEv resetBtn) (Behaviour.map (box (`diffTime` zeroTime)) startElapsedTime)

  -- Combine reset and max events
  let combinedInput = Event.interleave (box (\_ m -> m)) resetEv maxEv
  let timer = switchR initialTimer combinedInput

  -- UI
  maxText <- mkLabel' (Behaviour.map (box (\(_ :* max) -> "Max: " <> toText max)) timer)
  text <- mkLabel' (Behaviour.map (box (\(t :* _) -> "Current: " <> toText (nominalToInt t))) timer)
  pb <- mkProgressBar' (constK 0) maxBeh (Behaviour.map (box (\(curr :* _) -> nominalToInt curr)) timer)
  mkConstVStack' $ maxSlider :* maxText :* text :* resetBtn :* pb

main :: IO ()
main = runApplication' timerExample