{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Widgets
import WidgetRattus
import Behaviour
import Event
import Primitives
import Data.Text (Text)
import Prelude hiding (zipWith3, const, map)

import WidgetRattus.Widgets (setEnabled)

isDate :: Text -> Bool
isDate txt = case splitOn' "-" txt of
  [dayStr, monthStr, yearStr] ->
    let day = readMaybe' dayStr
        month = readMaybe' monthStr
        year = readMaybe' yearStr
    in isValid day month year
  _ -> False
  where
    isValid :: Maybe' Int -> Maybe' Int -> Maybe' Int -> Bool
    isValid (Just' d) (Just' m) (Just' y)
      | m < 1 || m > 12 = False
      | d < 1 || d > daysInMonth m y = False
      | otherwise = True
    isValid _ _ _ = False

    daysInMonth :: Int -> Int -> Int
    daysInMonth m y
        | m `elem` ([4, 6, 9, 11] :: List Int) = 30
        | m == 2 = if isLeapYear y then 29 else 28
        | otherwise = 31

    isLeapYear :: Int -> Bool
    isLeapYear y = y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)

isLater :: Text -> Text -> Bool
isLater dep ret = case (splitOn' "-" dep, splitOn' "-" ret) of
  ([depDayStr, depMonthStr, depYearStr], [retDayStr, retMonthStr, retYearStr]) ->
    let depDay = readMaybe' depDayStr
        depMonth = readMaybe' depMonthStr
        depYear = readMaybe' depYearStr
        retDay = readMaybe' retDayStr
        retMonth = readMaybe' retMonthStr
        retYear = readMaybe' retYearStr
    in all isJust' ([depDay, depMonth, depYear, retDay, retMonth, retYear] :: List (Maybe' Int)) &&
       (depYear < retYear ||
       (depYear == retYear && (depMonth < retMonth ||
       (depMonth == retMonth && depDay < retDay))))
  _ -> False
  
bookingToText :: Bool -> Text -> Text -> Text
bookingToText oneWay dep ret =
  "You have booked a " <> if oneWay then "one-way flight on " <> dep
  else "return flight from " <> dep <> " to " <> ret

flightBooker :: C VStack'
flightBooker = do
      dropDown <- mkTextDropdown' (const (K ["One-Way", "Return-Flight"])) "One-Way"
      tf1 <- mkTextField' "01-01-2021"
      tf2 <- mkTextField' "01-02-2021"
      button <- mkButton' (Behaviour.const (K ("Book" :: Text)))
      
      let isRF = Behaviour.map (box (== "Return-Flight")) (tddCurr dropDown)
      let isOW = Behaviour.map (box (== "One-Way")) (tddCurr dropDown)
      
      let labelBeh = zipWith3 (box bookingToText) isOW (tfContent tf1) (tfContent tf2)

      let beh = scan (box (\_ _ -> True)) False (btnOnClick button)
      
      label <- mkLabel' labelBeh
      label' <- mkOldWidget label
      popup <- mkPopup' (Event.stepper beh False) (const (K label'))

      let tf1IsDate = Behaviour.map (box isDate) (tfContent tf1)
      let tf1IsLater = Behaviour.zipWith (box isLater) (tfContent tf1) (tfContent tf2)

      let oneWayAndDate = Behaviour.zipWith (box (&&)) isOW tf1IsDate
      let returnFlightAndIsLater = Behaviour.zipWith (box (&&)) isRF tf1IsLater
      let validBooking = Behaviour.zipWith (box (||)) oneWayAndDate returnFlightAndIsLater

      isRF' <- discretize isRF
      tf2' <- mkOldWidget tf2
      validBooking' <- discretize validBooking
      button' <- mkOldWidget button
      mkConstVStack' (popup :* dropDown :* tf1 :* setEnabled tf2' isRF' :* setEnabled button' validBooking')

main :: IO ()
main = runApplication' flightBooker