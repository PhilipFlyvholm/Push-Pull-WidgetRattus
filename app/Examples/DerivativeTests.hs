{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Main where

import Behaviour (map, elapsedTime, derivative', constK, Beh)
import WidgetRattus
import Widgets
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

derivativeTests :: C VStack'
derivativeTests = do
  time <- elapsedTime
  time' <- derivative' (Behaviour.map (box realToFrac) time)
  let shouldBe = constK 1
  originalLbl <- mkLabel' time
  resultLbl <- mkLabel' (Behaviour.map (box toText) time')

  shouldLbl <- mkLabel' (Behaviour.map (box toText) shouldBe)

  let constantTest :: (Beh Float) = constK 514
  constantTest' <- derivative' constantTest
  constantResultLbl <- mkLabel' (Behaviour.map (box toText) constantTest')
  constantShouldLbl <- mkLabel' (Behaviour.map (box toText) (constK 0))


  -- UI
  mkConstVStack' $ originalLbl :* resultLbl :* shouldLbl :* constantResultLbl :* constantShouldLbl 

main :: IO ()
main = runApplication' derivativeTests