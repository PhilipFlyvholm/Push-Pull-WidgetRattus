module Main where

import Examples.Test (window)
import Widgets (runApplication')

main :: IO ()
main = runApplication' window
