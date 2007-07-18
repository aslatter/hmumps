module Main where

import LineAST

import Control.Monad
import Text.ParserCombinators.Parsec
import System.IO


main :: IO ()
main = hSetBuffering stdout NoBuffering >> splash >> loop 

loop = do 
       putStr "> "
       x <- getLine
       case x of
          '!':xs -> interpreterCommands xs loop
	  _ -> (repl . strip) x >> loop


interpreterCommands :: String -> IO () -> IO ()
interpreterCommands "q" _    = return ()
interpreterCommands "w" next = mapM_ putStrLn 
 ["  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY",
  "APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT",
  "HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY",
  "OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,",
  "THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR",
  "PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM",
  "IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF",
  "ALL NECESSARY SERVICING, REPAIR OR CORRECTION."] >> next
interpreterCommands str next = putStrLn ("Unkown interpreter command: " ++ str) >> next

repl :: String -> IO ()
repl [] = return ()
repl x = putStrLn $ case parse command "" x of
                      Left err -> show err
                      Right expTree -> show expTree

splash :: IO ()
splash = mapM_ putStrLn 
 ["HMUMPS  Copyright (C) 2007  Antoine Latter, Creighton Hogg",
  "This program comes with ABSOLUTELY NO WARRANTY; for details type `!w'.",
  "This is free software, and you are welcome to redistribute it",
  "under certain conditions; for details see the enclosed LISCENSE file.",
  ""]
