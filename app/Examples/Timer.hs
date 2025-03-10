{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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

createStop :: NominalDiffTime -> (NominalDiffTime :* Int -> Beh (NominalDiffTime :* Int))
createStop timeAtResetEvent (_ :* max) = stopWith (box (\(currentTime :* max') -> if nominalToInt currentTime >= max' then Just' (intToNominal max' :* max') else Nothing')) (timeFromLastTimer timeAtResetEvent max)

createStopMax :: Int -> NominalDiffTime -> (NominalDiffTime :* Int -> Beh (NominalDiffTime :* Int))
createStopMax max timeAtResetEvent (v :* _) =
  stopWith
    ( box
        ( \(currentTime :* max') ->
            if nominalToInt currentTime >= max'
              then Just' (intToNominal max' :* max')
              else Nothing'
        )
    )
    (const (Fun (box (\t -> (diffTime t (timeFromDiff (timeAtResetEvent - v)) :* max) :* False))))

timerExample :: C VStack'
timerExample = do
  let startElapsedTime = timeBehaviour
  let initalMax = 50
  maxSlider <- mkSlider' initalMax (constK 1) (constK 100)
  let maxBeh = sldCurr maxSlider
  let maxChangeEv = sliderOnChange maxSlider

  resetBtn <- mkButton' $ mkConstText "Reset timer"

  let resetEv :: Ev ((NominalDiffTime :* Int) -> Beh (NominalDiffTime :* Int)) =
        Event.triggerAwait (box (\_ timeAtResetEvent -> createStop timeAtResetEvent)) (btnOnClickEv resetBtn) (Behaviour.map (box (`diffTime` zeroTime)) startElapsedTime)

  let maxEv :: Ev ((NominalDiffTime :* Int) -> Beh (NominalDiffTime :* Int)) =
        Event.triggerAwait (box createStopMax) maxChangeEv (Behaviour.map (box (`diffTime` zeroTime)) startElapsedTime)

  now <- time
  let input = Event.interleave (box (\_ m -> m)) resetEv maxEv
  let timer = switchR (createStop (diffTime now zeroTime) (0 :* initalMax)) input

  maxText' <- mkLabel' (Behaviour.map (box (\max -> "Raw-Max: " <> toText max)) maxBeh)
  maxText <- mkLabel' (Behaviour.map (box (\(_ :* max) -> "Max: " <> toText max)) timer)
  text <- mkLabel' (Behaviour.map (box (\(t :* _) -> "Current: " <> toText (nominalToInt t))) timer)
  pb <- mkProgressBar' (constK 0) maxBeh (Behaviour.map (box (\(curr :* _) -> nominalToInt curr)) timer)
  mkConstVStack' $ maxText' :* maxText :* maxSlider :* text :* resetBtn :* pb

main :: IO ()
main = runApplication' timerExample