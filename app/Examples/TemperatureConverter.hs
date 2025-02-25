{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}

import WidgetRattus
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text hiding (zipWith, filter, map, all)
import Data.Text.Read
import Widgets
import Behaviour
import Primitives
import Event

-- Benchmark 2
celsiusToFahrenheit :: Int -> Int
celsiusToFahrenheit t = t * 9 `div` 5 + 32

fahrenheitToCelsius :: Int -> Int
fahrenheitToCelsius t = (t - 32) * 5 `div` 9

isNumber :: Text -> Maybe' Int
isNumber "" = Just' 0
isNumber t =
    case signed decimal t of
        Right (t', "") -> Just' t'
        _ -> Nothing'

window :: C HStack'
window = do
    tfF1 <- mkTextField' "32"
    tfC1 <- mkTextField' "0"

    let fEvent = Event.filterMap (box isNumber) (textFieldOnInput tfF1)
    let cEvent = Event.filterMap (box isNumber) (textFieldOnInput tfC1)

    let convertFtoC = Event.map (box fahrenheitToCelsius) fEvent
    let convertCtoF = Event.map (box celsiusToFahrenheit) cEvent

    let c = stepper 0 (interleave (box (\x _ -> x)) cEvent convertFtoC)
    let f = stepper 32 (interleave (box (\x _ -> x)) fEvent convertCtoF)

    let tfF2 = setInputBehTF tfF1 (Behaviour.map (box toText) f)
    let tfC2 = setInputBehTF tfC1 (Behaviour.map (box toText) c)

    fLabel <- mkLabel' (const (K ("Fahrenheit" :: Text)))
    cLabel <- mkLabel' (const (K ("Celsius" :: Text)))

    fStack <- mkConstVStack' (tfF2 :* fLabel)
    cStack <- mkConstVStack' (tfC2 :* cLabel)
    mkConstHStack' (fStack :* cStack)


main :: IO ()
main = runApplication' window