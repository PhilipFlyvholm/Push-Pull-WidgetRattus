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
import WidgetRattus.Signal (Sig((:::)))
import Event

-- Benchmark 2
celsiusToFahrenheit :: Text -> Text
celsiusToFahrenheit t =
        case signed decimal t of
            Right (t', _) -> toText (t' * 9 `div` 5 + 32)
            Left _ -> "Invalid input"

fahrenheitToCelsius :: Text -> Text
fahrenheitToCelsius t =
    case signed decimal t of
        Right (t', _) -> toText ((t' - 32) * 5 `div` 9)
        Left _ -> "Invalid input"

addInputBehTF :: TextField' -> Beh Text -> TextField'
addInputBehTF tf (Beh(_ ::: xs)) =
    let Beh(y ::: ys) = tfContent tf
        -- didnt work with behaviours out of the box
        -- rest = zipWith (box (\x _ -> x)) b' b
        -- instead we have used future beh as ev
        EvDense rest = Event.interleave (box (\x _ -> x)) (EvDense ys) (EvDense xs)
        content = y ::: rest
    in tf{tfContent = Beh content, tfInput = tfInput tf}

window :: C HStack'
window = do
    tfF1 <- mkTextField' "32"
    tfC1 <- mkTextField' "0"

    let convertFtoC = Behaviour.map (box fahrenheitToCelsius) (tfContent tfF1)
    let convertCtoF = Behaviour.map (box celsiusToFahrenheit) (tfContent tfC1)

    let tfF2 = addInputBehTF tfF1 convertCtoF
    let tfC2 = addInputBehTF tfC1 convertFtoC

    fLabel <- mkLabel' (const (K ("Fahrenheit" :: Text)))
    cLabel <- mkLabel' (const (K ("Celsius" :: Text)))

    fStack <- mkConstVStack' (tfF2 :* fLabel)
    cStack <- mkConstVStack' (tfC2 :* cLabel)
    mkConstHStack' (fStack :* cStack)


main :: IO ()
main = runApplication' window