{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , UndecidableInstances
           , GeneralizedNewtypeDeriving
  #-}

module System.Console.Haskeline.Class
    (HaskelineT
    ,runHaskelineT
    ,runHaskelineTWithPrefs
    ,MonadHaskeline(..)
    ,H.Settings(..)
    ,H.defaultSettings
    ,H.setComplete
    ,H.Prefs()
    ,H.readPrefs
    ,H.defaultPrefs
    ,H.Interrupt(..)
    ,H.handleInterrupt
    ,module System.Console.Haskeline.Completion
    ,module System.Console.Haskeline.MonadException
     ) where

import qualified System.Console.Haskeline as H
import System.Console.Haskeline.Completion
import System.Console.Haskeline.MonadException

import Control.Applicative
import Control.Monad.State

newtype HaskelineT m a = HaskelineT {unHaskeline :: H.InputT m a}
 deriving (Monad, Functor, Applicative, MonadIO, MonadException, MonadTrans, MonadHaskeline)

runHaskelineT :: MonadException m => H.Settings m -> HaskelineT m a -> m a
runHaskelineT s m = H.runInputT s (unHaskeline m)

runHaskelineTWithPrefs :: MonadException m => H.Prefs -> H.Settings m -> HaskelineT m a -> m a
runHaskelineTWithPrefs p s m = H.runInputTWithPrefs p s (unHaskeline m)

class MonadException m => MonadHaskeline m where
    getInputLine :: String -> m (Maybe String)
    getInputChar :: String -> m (Maybe Char)
    outputStr :: String -> m ()
    outputStrLn :: String -> m ()


instance MonadException m => MonadHaskeline (H.InputT m) where
    getInputLine = H.getInputLine
    getInputChar = H.getInputChar
    outputStr = H.outputStr
    outputStrLn = H.outputStrLn


instance MonadState s m => MonadState s (HaskelineT m) where
    get = lift get
    put = lift . put

instance MonadHaskeline m => MonadHaskeline (StateT s m) where
    getInputLine = lift . getInputLine
    getInputChar = lift . getInputChar
    outputStr = lift . outputStr
    outputStrLn = lift . outputStrLn

