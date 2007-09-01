{-# OPTIONS -fglasgow-exts -Wall -Werror -cpp #-}

#define MK_ACCESSOR(f) (Accessor f (\x s -> s {f = x}))

module HMumps.Runtime where

import Prelude hiding (lookup)
import qualified Prelude as P

import Data.Map
import Data.MValue
import Data.MArray
import Data.Accessor

import qualified HMumps.SyntaxTree as M
import HMumps.SyntaxTree
import HMumps.Parsers

import Control.Monad.State
import Control.Monad.Maybe

type Routine = Map String [Line]
type Line    = (Int, [M.Command])

data RunState = RunState {env_       :: Env,
                          linelevel_ :: Int,
                          tags_      :: Routine,
                          stack_     :: Maybe RunState}

env :: Accessor RunState Env
env = MK_ACCESSOR(env_)

linelevel :: Accessor RunState Int
linelevel = MK_ACCESSOR(linelevel_)

tags :: Accessor RunState Routine
tags = MK_ACCESSOR(tags_)

stack :: Accessor RunState (Maybe RunState)
stack = MK_ACCESSOR(stack_)


emptyState :: RunState
emptyState = RunState (NormalFrame empty) 0 empty Nothing

data Env = NoFrame
         | NormalFrame (Map String MArray)
         | StopFrame (Map String (Maybe MArray))

-- |A wrapper around fetch which will return mEmpty instead of failing.
fetch' :: MonadState RunState m => String -> m MArray
fetch' label = do result <- runMaybeT $ fetch label
                  case result of
                    Just ma -> return ma
                    Nothing -> return $ mEmpty

-- |Given the name of a local, returns the corresponding MArray or fails.
fetch :: MonadState RunState m => String -> m MArray
fetch label = do ev <- getA env
                 case ev of
                   NoFrame -> lookback
                   NormalFrame m -> case label `lookup` m of
                                      Nothing -> lookback
                                      Just ma -> return ma
                   StopFrame m   -> case label `lookup` m of
                                      Nothing -> fail $ "lookup failed: " ++ label
                                      Just Nothing -> lookback
                                      Just (Just ma) -> return ma
 where lookback :: MonadState RunState m => m MArray
       lookback = do Just rs <- getA stack
                     return $ evalState (fetch label) rs

-- |Given the name of a local, sets it to the given MArray.  May fail if the bottom of the stack doesn't
-- have a symbol table.
set :: MonadState RunState m => String -> MArray -> m ()
set label ma = do ev <- getA env
                  case ev of
                    NoFrame -> lookback
                    NormalFrame m -> case label `member` m of
                                       False -> lookback
                                       True  -> putA env (NormalFrame (insert label ma m))
                    StopFrame m -> case label `lookup` m of
                                     Just Nothing -> lookback
                                     _ -> putA env (StopFrame (insert label (Just ma) m))
 where lookback :: MonadState RunState m => m ()
       lookback = do Just rs <- getA stack
                     putA stack (Just (execState (set label ma) rs))

eval :: MonadState RunState m => M.Expression -> m MValue
eval (ExpLit m) = return m
eval (ExpVn vn) = case vn of
                    Lvn label subs -> do ma <- fetch' label
                                         mvs <- mapM eval subs
                                         case mIndex ma mvs of
                                           Just mv -> return mv
                                           Nothing -> return $ String ""
                    Gvn _ _ -> fail "Globals not yet implemented"
                    IndirectVn expr subs -> do result <- eval expr
                                               let String str = mString result
                                               case parse parseVn "Indirect VN" str of
                                                 Right (Lvn label subs') -> eval $ ExpVn $ Lvn label (subs' ++ subs)
                                                 Right (Gvn label subs') -> eval $ ExpVn $ Gvn label (subs' ++ subs)
                                                 Right (IndirectVn expr' subs') -> eval $ ExpVn $ IndirectVn expr' (subs' ++ subs)
                                                 Left err -> fail . show $ err
eval (ExpUnop unop expr) = do mv <- eval expr
                              return $ case unop of
                                UNot   -> mNot   mv
                                UPlus  -> mPlus  mv
                                UMinus -> mMinus mv
eval (ExpBinop binop lexp rexp) 
 = do lv <- eval lexp
      rv <- eval rexp
      return $ case binop of
        Concat      -> lv `mConcat` rv
        Add         -> lv + rv
        Sub         -> lv - rv
        Mult        -> lv * rv
        Div         -> lv / rv
        Rem         -> lv `mRem` rv
        Quot        -> lv `mQuot` rv
        Pow         -> lv ** rv
        And         -> lv `mAnd` rv
        Or          -> lv `mOr` rv
        Equal       -> lv `mEqual` rv
        LessThan    -> lv `mLT` rv
        GreaterThan -> lv `mGT` rv
        Follows     -> lv `follows` rv
        Contains    -> lv `contains` rv
        SortsAfter  -> boolToM $ lv > rv
eval (Pattern _ _)   = undefined
eval (FunCall _ _ _) = undefined
eval (BIFCall _ _)   = undefined

