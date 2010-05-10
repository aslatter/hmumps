{-# OPTIONS -Wall -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

-- import Text.Parsec
import System.IO
import System.Console.Haskeline.Class
import Control.Monad.State
import Control.Monad.Error

import HMumps.Runtime
import HMumps.Parsers

import Templates

main :: IO ()
main = do
  -- hSetBuffering stdout NoBuffering
  putStrLn splash
  runHaskelineT defaultSettings $ runStateT loop emptyState
  return ()

loop :: (MonadState [RunState] m, MonadHaskeline m) => m ()
loop = do line <- getInputLine "> "
          case line of
            Just x -> if x == ""
                      then loop
                      else 
                              case x of
                                '!':xs -> interpreterCommands xs loop
	                        _ -> (repl . strip) x >> loop
            Nothing -> liftIO (putStrLn "") >> return ()


interpreterCommands :: (MonadIO m, MonadState [RunState] m) => String -> m () -> m ()
interpreterCommands "q" _    = return ()
interpreterCommands "w" next = (liftIO $ putStrLn warranty) >> next
interpreterCommands str next = (liftIO $ putStrLn $ "Unkown interpreter command: " ++ str) >> next

repl :: (MonadState [RunState] m, MonadIO m) => String -> m ()
repl [] = return ()
repl x = do
  case parse parseCommands "" x of
    Left err -> do
           liftIO $ putStrLn $ show err
           modify (take 1)
    Right xs -> do
           result <- step (exec xs >> liftIO (putChar '\n') >> setX 0 >> addY 1)
           case result of
             Right _ -> return ()
             Left str -> liftIO $ putStrLn str

splash :: String
splash = $(bakedString "SPLASH")

warranty :: String
warranty = $(bakedString "WARRANTY")
