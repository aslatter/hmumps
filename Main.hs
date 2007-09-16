{-# OPTIONS -Wall -Werror #-}

module Main where

import HMumps.Parsers

import Text.ParserCombinators.Parsec
import System.IO
import System.Console.Readline -- Note, uses GNU readline, under GPL
import Control.Monad.State
import Data.Map

import HMumps.Runtime

main :: IO ()
main = hSetBuffering stdout NoBuffering >> putStrLn splash >> runStateT loop emptyState >> return ()

loop :: (MonadState [RunState] m, MonadIO m) => m ()
loop = do line <- liftIO $ readline "> "
          case line of
            Just x -> do liftIO $ addHistory x
                         case x of
                           '!':xs -> interpreterCommands xs loop
	                   _ -> (repl . strip) x >> loop
            Nothing -> liftIO (putStrLn "") >> return ()


interpreterCommands :: (MonadIO m, MonadState [RunState] m) => String -> m () -> m ()
interpreterCommands "q" _    = return ()
interpreterCommands "w" next = (liftIO $ mapM_ putStrLn 
 ["  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY",
  "APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT",
  "HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY",
  "OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,",
  "THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR",
  "PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM",
  "IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF",
  "ALL NECESSARY SERVICING, REPAIR OR CORRECTION."]) >> next
interpreterCommands "lvns" next = do ev <- (env . head) `liftM` get
                                     case ev of
                                       NoFrame -> next
                                       NormalFrame m -> mapM_ (liftIO . putStrLn . show) (keys m) >> next
                                       StopFrame m -> mapM_ (liftIO . putStrLn . show) (keys m) >> next
interpreterCommands str next = (liftIO $ putStrLn $ "Unkown interpreter command: " ++ str) >> next

repl :: (MonadState [RunState] m, MonadIO m) => String -> m ()
repl [] = return ()
repl x = case parse parseCommands "" x of
           Left err -> liftIO $ putStrLn $ show err
           Right xs -> exec xs >> return ()


splash :: String
splash = concat 
 ["HMUMPS  Copyright (C) 2007  Antoine Latter, Creighton Hogg\n",
  "This program comes with ABSOLUTELY NO WARRANTY; for details type `!w'.\n",
  "This is free software, and you are welcome to redistribute it\n",
  "under certain conditions; for details see the enclosed LICENSE file.\n",
  ""]
