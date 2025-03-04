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
      flightTypeDropdown <- mkTextDropdown' (const (K ["One-Way", "Return-Flight"])) "One-Way"
      departureDateField <- mkTextField' "01-01-2021"
      returnDateField <- mkTextField' "01-02-2021"
      bookButton <- mkButton' (Behaviour.const (K ("Book" :: Text)))
      
      let isReturnFlight = Behaviour.map (box (== "Return-Flight")) (tddCurr flightTypeDropdown)
      let isOneWayFlight = Behaviour.map (box (== "One-Way")) (tddCurr flightTypeDropdown)
      
      let bookingSummary = zipWith3 (box bookingToText) isOneWayFlight (tfContent departureDateField) (tfContent returnDateField)

      let triggerPopup = scan (box (\_ _ -> True)) False (btnOnClickEv bookButton)
      
      summaryLabel <- mkLabel' bookingSummary
      summaryLabel' <- mkOldWidget summaryLabel
      summaryPopup <- mkPopup' triggerPopup (const (K summaryLabel'))

      let departureDateFieldIsDate = Behaviour.map (box isDate) (tfContent departureDateField)
      let departureDateFieldIsLater = Behaviour.zipWith (box isLater) (tfContent departureDateField) (tfContent returnDateField)

      let oneWayAndDate = Behaviour.zipWith (box (&&)) isOneWayFlight departureDateFieldIsDate
      let returnFlightAndIsLater = Behaviour.zipWith (box (&&)) isReturnFlight departureDateFieldIsLater
      let validBooking = Behaviour.zipWith (box (||)) oneWayAndDate returnFlightAndIsLater

      isReturnFlight' <- discretize isReturnFlight
      returnDateField' <- mkOldWidget returnDateField
      validBooking' <- discretize validBooking
      bookButton' <- mkOldWidget bookButton
      
      mkConstVStack' (summaryPopup :* flightTypeDropdown :* departureDateField :* setEnabled returnDateField' isReturnFlight' :* setEnabled bookButton' validBooking')

main :: IO ()
main = runApplication' flightBooker