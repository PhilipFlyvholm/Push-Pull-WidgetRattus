{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Main where

import Behaviour (integral', map, elapsedTime, derivative')
import WidgetRattus
import Widgets
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

integralTests :: C VStack'
integralTests = do
  time <- elapsedTime
  time' <- integral' 0 (Behaviour.map (box realToFrac) time)
  derivativeTime <- derivative' time'
  let shouldBe = Behaviour.map (box (\t -> fromRational ((toRational t)^2)/2)) time
  originalLbl <- mkLabel' time
  resultLbl <- mkLabel' (Behaviour.map (box toText) time')

  shouldLbl <- mkLabel' (Behaviour.map (box toText) shouldBe)
  derivativeLbl <- mkLabel' (Behaviour.map (box toText) derivativeTime)
  -- UI
  mkConstVStack' $ originalLbl :* resultLbl :* shouldLbl :* derivativeLbl

main :: IO ()
main = runApplication' integralTests