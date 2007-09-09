{-# OPTIONS -fglasgow-exts -Wall -Werror #-}

module HMumps.Runtime where

import Prelude hiding (lookup)
import qualified Prelude as P

import Data.Map
import Data.MValue
import Data.MArray

import HMumps.Routine
import HMumps.SyntaxTree
import HMumps.Parsers

import Control.Monad.State
import Control.Monad.Maybe


data RunState = RunState {env       :: Env,
                          linelevel :: Int,
                          tags      :: Routine}

emptyState :: [RunState]
emptyState = [RunState NoFrame 0 empty]

data Env = NoFrame
         | NormalFrame (Map String MArray)
         | StopFrame (Map String (Maybe MArray))


fetch :: String -> [RunState] -> Maybe MArray
fetch str xs = join $ foldr helper Nothing (P.map (look str . env) xs)

 where helper :: Maybe (Maybe MArray) -> Maybe (Maybe MArray) -> Maybe (Maybe MArray)
       helper x Nothing      = x
       helper _ j@(Just _)   = j

look :: String -> Env -> Maybe (Maybe MArray)
look _ NoFrame = Nothing
look str (NormalFrame m) = case lookup str m of
                             Nothing -> Nothing
                             j       -> Just j
look str (StopFrame m) = case lookup str m of
                           Nothing      -> Just Nothing
                           Just Nothing -> Nothing
                           x            -> x

-- |Returns the MArray associated with the named local var, or the empty MArray
fetch' :: MonadState [RunState] m => String -> m MArray
fetch' str = do result <- (fetch str) `liftM` get
                case result of
                  Just x  -> return x
                  Nothing -> return mEmpty


set' :: String -> MArray -> [RunState] -> [RunState]
set' _ _ []        = error "set called with an empty stack"
set' str ma (x:[]) = case (env x) of
                      NoFrame -> x {env = (NormalFrame (insert str ma empty))} : []
                      NormalFrame m -> x {env = (NormalFrame (insert str ma m))} : []
                      StopFrame m -> x {env = (StopFrame (insert str (Just ma) m))} : []
set' str ma (x:xs) = case (look str . env) x of 
                      Nothing -> x : (set' str ma xs)
                      Just _ -> case env x of
                                  NoFrame -> error "something bad happened in HMumps.Runtime.set"
                                  NormalFrame m -> x {env = (NormalFrame (insert str ma m))} : xs
                                  StopFrame m -> x {env = (StopFrame (insert str (Just ma) m))} : xs


set :: MonadState [RunState] m => String -> MArray -> m ()
set str ma = modify (set' str ma)


exec :: MonadState [RunState] m => Line -> m ()
exec = error "HMumps.Runtime.exec: undefined"


eval :: MonadState [RunState] m => Expression -> m MValue
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
                                UPlus  -> mNum   mv
                                UMinus -> negate mv
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
eval (Pattern _ _)   = fail "Can't evaluate pattern matches"
eval (FunCall _ _ _) = fail "Can't evaluate function calls"
eval (BIFCall _ _)   = fail "Can't evaluate built-in function calls"

