{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Behaviour where

import Primitives
import WidgetRattus
import WidgetRattus.Signal
import Prelude hiding (map)

data Beh a = Beh !(Sig (Fun Time a))

timeBehaviour :: Beh Time
timeBehaviour = Beh (Fun (box id) ::: never)

map :: Box (a -> b) -> Beh a -> Beh b
map f (Beh (x ::: xs)) = Beh (mapF f x ::: delay (let Beh s = Behaviour.map f (Beh (adv xs)) in s))

-- Not 100% correct since it only uses the inital time and not future times
discretize :: Beh a -> C (Sig a)
discretize (Beh sig) = do
    t <- time
    return $ WidgetRattus.Signal.map (box (\ ft -> apply ft t)) sig

timer :: C (Beh NominalDiffTime)
timer = do
    startTime <- time
    return $ Beh (Fun (box (\currentTime -> diffTime startTime currentTime)) ::: never)