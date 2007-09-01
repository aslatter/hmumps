{-# OPTIONS -fglasgow-exts -Wall -Werror #-}

module Data.Accessor
 (Accessor(Accessor),
  getVal,
  setVal,
  (@.),
  getA,
  putA,
  modA,
  at) where

import Control.Monad.State

data Accessor s a = 
     Accessor { getVal :: s -> a
              , setVal :: a -> s -> s
              }
             
(@.) :: Accessor b c -> Accessor a b -> Accessor a c
(@.) f g = 
    Accessor { getVal = getVal f . getVal g
             , setVal = \c a -> setVal g (setVal f c (getVal g a)) a
             }

getA :: MonadState s m => Accessor s a -> m a
getA acc = get >>= (return . getVal acc)

putA :: MonadState s m => Accessor s a -> a -> m ()
putA acc x = get >>= put . setVal acc x

modA :: MonadState s m => Accessor s a -> (a -> a) -> m ()
modA acc f = do
    getA acc >>= (return . f) >>= putA acc

at :: Int -> Accessor [a] a
at n = Accessor (!! n) (\x a -> take n a ++ [x] ++ drop (n+1) a)
