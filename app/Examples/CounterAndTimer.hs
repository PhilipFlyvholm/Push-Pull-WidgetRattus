-- {-# HLINT ignore "Evaluate" #-}
-- {-# HLINT ignore "Use const" #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- {-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

-- import Behaviour
-- import Event
-- import WidgetRattus

-- import Widgets 
-- import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

-- counterAndTimer :: C VStack'
-- counterAndTimer = do
--   elapsedTime' <- Behaviour.elapsedTime
--   text <- mkLabel' elapsedTime'
--   btn <- mkButton' timeBehaviour
--   let ev = btnOnClick btn
--   lbl <- mkLabel' $ Event.scanB (box (\n _ -> n + 1 :: Int)) 0 ev
--   mkConstVStack' (text :* lbl :* btn)

main :: IO ()
main = do
  print "hallo"
-- main = runApplication' counterAndTimer