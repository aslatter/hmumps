module Main where

import LineAST

import Control.Monad
import Text.ParserCombinators.Parsec
import System.IO

-- I want this to be a REPL, where the user is given a prompt.
-- But this doesnt do that ... it prints the prompt after taking
-- input.  I don't know why

main :: IO ()
main = disclaimer >> (loop $ do putStr "> "
                                hFlush stdout
                                x <- getLine
                                putStrLn $ repl x)

loop :: Monad m => m () -> m ()
loop ma = ma >> (loop ma)

repl :: String -> String
repl x = case parse command "" x of
         Left err -> show err
         Right expTree -> show expTree

disclaimer :: IO ()
disclaimer = do putStrLn "HMUMPS  Copyright (C) 2007  Antoine Latter, Creighton Hogg"
                putStrLn "This program comes with ABSOLUTELY NO WARRANTY; for details see"
                putStrLn "the enclosed LISCENSE file."
                putStrLn "This is free software, and you are welcome to redistribute it"
                putStrLn "under certain conditions; for details see the enclosed LISCENSE file."
                putStrLn ""