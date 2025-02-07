{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
module Event where

import Behaviour
import Primitives (apply)
import WidgetRattus
import WidgetRattus.Signal

newtype Ev a = Ev (O (Sig a))

triggerAwait :: (Stable b) => Box (a -> b -> c) -> Ev a -> Beh b -> Ev (Maybe' c)
triggerAwait f event behaviour = Ev (trig f event behaviour)
  where
    trig :: (Stable b) => Box (a -> b -> c) -> Ev a -> Beh b -> O (Sig (Maybe' c))
    trig f' (Ev as) (Beh (b ::: bs)) =
      delayC $
        delay
          ( let d = select as bs
             in ( do
                    t <- time
                    return
                      ( case d of
                          Fst (a' ::: as') bs' -> Just' (unbox f' a' (apply b t)) ::: trig f' (Ev as') (Beh (b ::: bs'))
                          Snd as' bs' -> Nothing' ::: trig f' (Ev as') (Beh bs')
                          Both (a' ::: as') (b' ::: bs') -> Just' (unbox f' a' (apply b' t)) ::: trig f' (Ev as') (Beh (b' ::: bs'))
                      )
                )
          )