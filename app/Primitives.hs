{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}

module Primitives where

import WidgetRattus
import WidgetRattus.InternalPrimitives

data Fun a where
  K :: !a -> Fun a
  Fun :: (Stable s) => !s -> !(Box (s -> Time -> (Maybe' s :* a))) -> Fun a

continuous ''Fun

apply :: Fun a -> (Time -> a)
apply (K a) = Prelude.const a
apply (Fun s f) = \t -> let (_ :* a) = unbox f s t in a

mapF :: Box (a -> b) -> Fun a -> Fun b
mapF f (K a) = K (unbox f a)
mapF f (Fun s f') = Fun s (box (\s t -> let (s':* a) = unbox f' s t in (s' :* unbox f a)))

delayCF :: O(a -> C b) -> O(a -> b)
delayCF (Delay c f) = Delay c (\inp a -> advC' (f inp a) inp)


funTest :: Fun a -> O () -> O (Fun a)
funTest fun@(Fun x f) d = delay (let _ = adv d in x `seq` fun)

funTest2 :: Fun a -> O () -> O (Fun a)
funTest2 fun d = case fun of (!(Fun x f)) -> delay (let _ = adv d in x `seq` fun)

-- funTest3 :: C (Fun a) -> O () -> C (O (Fun a))
-- funTest3 fun d = do Fun x f <-  fun 
--                     fun' <- fun
--                     return (delay (let _ = adv d in x `seq` fun'))

funTest4 :: Fun a -> O () -> C (O (Fun a))
funTest4 fun@(Fun x f) d = do 
                              t <- time
                              let (x':* v) = unbox f x t
                              return (delay (let _ = adv d in x' `seq` fun))


zeroTime = (Time (toEnum 0) 0)

funTest5 :: Fun a -> O () -> O (Fun a)
funTest5 fun@(Fun x f) d = delay (let _ = adv d in x' `seq` fun)
  where (x':* v) = unbox f x zeroTime


funTest6 :: Fun a -> O () -> O (Fun a)
funTest6 fun@(Fun x f) d = let (x':* v) = unbox f x zeroTime in delay (let _ = adv d in x' `seq` fun)



funTestWorkaround :: Fun a -> O () -> O (Fun a)
funTestWorkaround fun@(Fun x f) d = foo x fun
  where foo :: Stable s => s -> (Fun a) -> O (Fun a)
        foo x fun = delay (let _ = adv d in x `seq` fun)