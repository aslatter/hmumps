{-# OPTIONS -fglasgow-exts -Wall -Werror #-}

module HMumps.Runtime where

import Prelude hiding (lookup)
import qualified Prelude as P

import Data.Map
import Data.MValue
import Data.MArray

import qualified HMumps.SyntaxTree as M
import HMumps.SyntaxTree
import HMumps.Parsers

import Control.Monad.State
import Control.Monad.Maybe

type Routine = Map String [Line]
type Line    = (Int, [M.Command])

data RunState = RunState {env       :: Env,
                          linelevel :: Int,
                          tags      :: Routine,
                          stack     :: Maybe RunState}

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
fetch label = do ev <- env `liftM` get
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
       lookback = do Just rs <- stack `liftM` get
                     return $ evalState (fetch label) rs

-- |Given the name of a local, sets it to the given MArray.  May fail if the bottom of the stack doesn't
-- have a symbol table.
set :: MonadState RunState m => String -> MArray -> m ()
set label ma = do ev <- env `liftM` get
                  case ev of
                    NoFrame -> lookback
                    NormalFrame m -> case label `member` m of
                                       False -> lookback
                                       True  -> do state <- get
                                                   put $ state {env= NormalFrame (insert label ma m)}
                    StopFrame m -> case label `lookup` m of
                                     Just Nothing -> lookback
                                     _ -> do state <- get
                                             put state {env= StopFrame (insert label (Just ma) m)}
 where lookback :: MonadState RunState m => m ()
       lookback = do state <- get
                     let Just rs = stack state
                     put $ state {stack= Just (execState (set label ma) rs)}

eval :: MonadState RunState m => M.Expression -> m MValue
eval (ExpLit m) = m
eval (ExpVn vn) = case vn of
                    Lvn label subs -> do ma <- fetch' label
                                         case mIndex ma subs of
                                           Just mv -> return mv
                                           Nothing -> return $ String ""
                    Gvn _ _ -> fail "Globals not yet implemented"
                    IndirectVn exp subs -> do result <- eval exp
                                              let String str = mString result
                                              case parse parseVn "Indirect VN" str of
                                                Right (Lvn label subs') -> eval $ ExpVn $ Lvn label (subs' ++ subs)
                                                Right (Gvn label subs') -> eval $ ExpVn $ Gvn laebl (subs' ++ subs)
                                                Right (IndirectVn exp' subs') -> eval $ ExpVn $ IndirectVn exp (subs' ++ subs)
                                                Left err -> fail . show err
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
        Rem         -> lv `rem` rv
        Quot        -> lv `quot` rv
        Pow         -> lv ^ rv
        And         -> lv `mAnd` rv
        Or          -> lv `mOr` rv
        Equal       -> lv `mEqual` rv
        LessThan    -> lv `mLT` rv
        GreaterThan -> lv `mGT` rv
        Follows     -> lv `follows` rv
        Contains    -> lv `contains` rv
        SortsAfter  -> lv > rv
                               
                                      