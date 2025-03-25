{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Main where

import Behaviour (intergral', map, elapsedTime)
import WidgetRattus
import Widgets
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

integralTests :: C VStack'
integralTests = do
  time <- elapsedTime
  time' <- intergral' 0 () (Behaviour.map (box realToFrac) time)
  let shouldBe = Behaviour.map (box (\t -> fromRational ((toRational t)^2)/2)) time
  originalLbl <- mkLabel' time
  resultLbl <- mkLabel' (Behaviour.map (box toText) time')

  shouldLbl <- mkLabel' (Behaviour.map (box toText) shouldBe)
  -- UI
  mkConstVStack' $ originalLbl :* resultLbl :* shouldLbl

main :: IO ()
main = runApplication' integralTests