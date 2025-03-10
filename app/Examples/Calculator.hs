{-# LANGUAGE OverloadedLists #-}{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Behaviour
import Event
import WidgetRattus
import Widgets

nums :: List Int
nums = [0 .. 9]

data Op = Plus | Minus | Equals | Reset

compute :: (Int :* Op :* Bool -> Maybe' (Int :* Op) -> Int :* Op :* Bool)
compute (n :* op     :* _) Nothing'          = (n :* op :* False)
compute _                  (Just' (_ :* Reset)) = (0 :* Reset :* True)
compute (n :* Plus   :* _) (Just' (m :* op)) = (n + m) :* op :* True
compute (n :* Minus  :* _) (Just' (m :* op)) = (n - m) :* op :* True
compute (_ :* Equals :* _) (Just' (m :* op)) = m :* op :* True
compute (_ :* Reset  :* _) (Just' (m :* op)) = m :* op :* True

calculatorExample :: C VStack'
calculatorExample = do
  numBtns :: List Button' <-
    mapM (mkButton' . constK) nums

  let [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9] = numBtns
  resetBut <- mkButton' (mkConstText "C")
  addBut <- mkButton' (mkConstText "+")
  subBut <- mkButton' (mkConstText "-")
  eqBut <- mkButton' (mkConstText "=")

  -- Event to construct numbers
  let numClicks :: List (Ev (Int -> Int)) =
        zipWith' (\b n -> Event.map (box (\_ x -> x * 10 + n)) (btnOnClickEv b)) numBtns nums
  -- signal to reset the current number to 0, after clicking an
  -- operator button
  let resetSig :: Ev (Int -> Int) =
        Event.map (box (\_ _ -> 0)) $
          interleaveAll (box (\a _ -> a)) $
            map' btnOnClickEv [addBut, subBut, eqBut, resetBut]

  -- combine signals to construct the number signal
  let sigList = resetSig :! numClicks :: List (Ev (Int -> Int))
  let combinedSig = interleaveAll (box (\a _ -> a)) sigList

  -- number signal (i.e. the multidigit number that has been
  -- constructed)
  let numberSig :: Ev Int =
        scan (box (\a f -> f a)) 0 combinedSig

  -- operator signal
  let opSig :: Ev Op =
          interleaveAll (box (\a _ -> a)) $
            map'
              (\(op :* btn) -> Event.map (box (\_ -> op)) (btnOnClickEv btn))
              [(Plus :* addBut), (Minus :* subBut), (Equals :* eqBut), (Reset :* resetBut)]

  -- signal consisting of an operand (i.e. a number) @n@ and an
  -- operator @op@. @n@ is the value of @numberSig@ just before
  -- clicking an operator button, and op is taken from opSig
  let operand :: Ev (Maybe' (Int :* Op)) =
        triggerAwaitM (box (\op n -> Just' (n :* op))) opSig (stepper 0 (buffer 0 numberSig))

  -- The result signal consisting of a number n that is the result
  -- of the current computation, an operator op that still needs to
  -- applied to n and a Boolean b that indicates whether we have
  -- just calculated n (and thus n should be displayed)
  let resSig :: Ev (Int :* Op :* Bool) =
        scan (box compute) (0 :* Plus :* True) operand

  -- -- The signal that should be displayed
  let displaySig :: Beh Int =
        Behaviour.zipWith (box (\(n :* _ :* b) m -> if b then n else m)) (Event.stepper (0 :* Plus :* False) resSig) (Event.stepper 0 numberSig)

  -- label to display the result (and operands)
  result <- mkLabel' displaySig

  operators <- mkConstVStack' (resetBut :* addBut :* subBut :* eqBut)
  row1 <- mkConstHStack' (b7 :* b8 :* b9)
  row2 <- mkConstHStack' (b4 :* b5 :* b6)
  row3 <- mkConstHStack' (b1 :* b2 :* b3)

  numbers <- mkConstVStack' (row1 :* row2 :* row3 :* b0)

  input <- mkConstHStack' (numbers :* operators)
  mkConstVStack' (result :* input)

main :: IO ()
main = runApplication' calculatorExample