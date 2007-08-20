{-# OPTIONS -fglasgow-exts -Wall -Werror #-}

module Runtime where

import Data.MValue
import Data.MArray

import qualified HMumps.SyntaxTree as M

import Control.Monad
import Control.Monad.State

type Routine = Map String [Line]
type Line    = (Int, [M.Commad])

newtype RunState = RunState {env       :: [Map String MArray],
                             linelevel :: Int,
                             cont      :: Cursor}
