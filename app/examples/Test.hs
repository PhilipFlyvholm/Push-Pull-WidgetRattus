{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Examples.Test (window) where

import Behaviour
import Data.Text hiding (all, filter, map)
import WidgetRattus
import WidgetRattus.Signal
import WidgetRattus.Widgets
  ( VStack,
    btnOnClickSig,
    mkButton,
    mkConstVStack,
    mkLabel,
  )
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

window :: C VStack
window = do
  elapsedTime' <- Behaviour.elapsedTime
  tSig <- discretize elapsedTime'

  text <- mkLabel tSig
  btn <- mkButton (const ("Increment" :: Text))
  let sig = btnOnClickSig btn
  let sig' = scanAwait (box (\n _ -> n + 1 :: Int)) 0 sig
  lbl <- mkLabel sig'
  mkConstVStack (text :* lbl :* btn)
