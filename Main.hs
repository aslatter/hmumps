module Main where

import LineAST

import Control.Monad
import Text.ParserCombinators.Parsec
import System.IO


main :: IO ()
main = hSetBuffering stdout NoBuffering >> disclaimer >> loop 

loop = do 
       putStr "> "
       x <- getLine
       case x of
          '!':xs -> interpreterCommands xs loop
	  _ -> (repl . strip) x >> loop

interpreterCommands :: String -> IO () -> IO ()
interpreterCommands "q" _    = return ()
interpreterCommands str next = putStrLn ("Unknown interpreter command: " ++ str) >> next

repl :: String -> IO ()
repl [] = return ()
repl x = putStrLn $ case parse command "" x of
                      Left err -> show err
                      Right expTree -> show expTree

disclaimer :: IO ()
disclaimer = do putStrLn "HMUMPS  Copyright (C) 2007  Antoine Latter, Creighton Hogg"
                putStrLn "This program comes with ABSOLUTELY NO WARRANTY; for details see"
                putStrLn "the enclosed LISCENSE file."
                putStrLn "This is free software, and you are welcome to redistribute it"
                putStrLn "under certain conditions; for details see the enclosed LISCENSE file."
                putStrLn ""
