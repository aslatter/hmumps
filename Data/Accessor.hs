{-# OPTIONS -fglasgow-exts -fth#-}

module Data.Accessor
 (Accessor(Accessor),
  getVal,
  setVal,
  getA,
  putA,
  modA,
  at,
  mkAccessor) where

import Control.Monad.State
import Language.Haskell.TH

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

mkAccessor f = do x <- newName "x"
                  s <- newName "s"
                  con <- [| Accessor |]
                  VarE fun <- [| f |]
                  return $ AppE (AppE (con) (VarE fun)) (LamE [VarP x,VarP s] (RecUpdE (VarE s) [(fun,VarE x)]))
