{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Primitives2 where

import WidgetRattus
import WidgetRattus.Signal (Sig ((:::)))

data Either' a b =
  Left' !a
  | Right' !b

data SimpleFun t a =
    K' !a
  | Fun' !(Box (t -> a))

data Fun t a =
  K !a
  | Fun !(Box (t -> a))
  | Switch !(Box (t -> Either' a (Sig (SimpleFun t a))))

continuous ''Fun

apply :: Fun t a -> t -> a
apply (K a) = Prelude.const a
apply (Fun f) = unbox f
apply (Switch f) = switchApp f
  where switchApp f t = 
          case unbox f t of
            Left' a -> a
            Right' (K' a ::: _) -> a
            Right' (Fun' f ::: _) -> unbox f t

simpleToFun :: SimpleFun t a -> Fun t a
simpleToFun (K' a) = K a 
simpleToFun (Fun' f) = Fun f

{-
mapF :: Box (a -> b) -> Fun t a -> Fun t b
mapF f (K a) = K (unbox f a)
mapF f (Fun f') = Fun (box (\t -> let (a :* b) = unbox f' t in (unbox f a :* b)))

mapFBool :: Box (a -> Bool) -> Fun t a -> Fun t a
mapFBool _ (K a) = K a
mapFBool g (Fun f') = Fun (box (\t -> let (a :* _) = unbox f' t in (a :* unbox g a)))-}