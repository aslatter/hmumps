{-# OPTIONS -fglasgow-exts -Wall -Werror #-}

module HMumps.Runtime where

import Prelude hiding (lookup)
import qualified Prelude as P

import Data.Map

import Data.MArray

import qualified HMumps.SyntaxTree as M

import Control.Monad.State
import Control.Monad.Maybe

type Routine = Map String [Line]
type Line    = (Int, [M.Command])

data RunState = RunState {env       :: Env,
                          linelevel :: Int,
                          tags      :: Routine,
                          stack     :: Maybe RunState}

emptyState :: RunSate
emptyState = RunSate NoFram 0 empty Nothing

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
                                      Nothing -> lookback
                                      Just Nothing -> fail $ "lookup failed: " ++ label
                                      Just (Just ma) -> return ma
 where lookback :: MonadState RunState m => m MArray
       lookback = do prev <- stack `liftM` get
                     case prev of
                       Nothing -> fail $ "lookup failed: " ++ label
                       Just rs -> return $ evalState (fetch label) rs
                  