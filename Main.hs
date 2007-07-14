module Main where

import LineAST

import Control.Monad
import Text.ParserCombinators.Parsec

-- I want this to be a REPL, where the user is given a prompt.
-- But this doesnt do that ... it prints the prompt after taking
-- input.  I don't know why

main :: IO ()
main = loop $ do putStr "> "
                 x <- getLine
                 putStrLn $ repl x

loop :: Monad m => m () -> m ()
loop ma = ma >> (loop ma)

repl :: String -> String
repl x = case parse command "" x of
         Left err -> show err
         Right expTree -> show expTree