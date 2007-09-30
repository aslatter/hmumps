{-# OPTIONS -Wall -Werror -fglasgow-exts -fth #-}

module Main where

import HMumps.Parsers

import Text.ParserCombinators.Parsec
import System.IO
import System.Console.Readline -- Note, uses GNU readline, under GPL
import Control.Monad.State
--import Data.Map

import HMumps.Runtime

import Templates

main :: IO ()
main = hSetBuffering stdout NoBuffering >> putStrLn splash >> runStateT loop emptyState >> return ()

loop :: (MonadState [RunState] m, MonadIO m) => m ()
loop = do line <- liftIO $ readline "> "
          case line of
            Just x -> if x == ""
                      then loop
                      else do liftIO $ addHistory x
                              case x of
                                '!':xs -> interpreterCommands xs loop
	                        _ -> (repl . strip) x >> loop
            Nothing -> liftIO (putStrLn "") >> return ()


interpreterCommands :: (MonadIO m, MonadState [RunState] m) => String -> m () -> m ()
interpreterCommands "q" _    = return ()
interpreterCommands "w" next = (liftIO $ putStrLn warranty) >> next
{-interpreterCommands "lvns" next = do ev <- (env . head) `liftM` get
                                     case ev of
                                       NoFrame -> next
                                       NormalFrame m -> mapM_ (liftIO . putStrLn) (keys m) >> next
                                       StopFrame m -> mapM_ (liftIO . putStrLn) (keys m) >> next -}
interpreterCommands str next = (liftIO $ putStrLn $ "Unkown interpreter command: " ++ str) >> next

repl :: (MonadState [RunState] m, MonadIO m) => String -> m ()
repl [] = return ()
repl x = case parse parseCommands "" x of
           Left err -> liftIO $ putStrLn $ show err
           Right xs -> exec xs >> liftIO (putChar '\n') >> setX 0 >> addY 1

splash :: String
splash = $(bakedString "SPLASH")

warranty :: String
warranty = $(bakedString "WARRANTY")
