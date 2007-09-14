{-# OPTIONS
            -Wall
            -Werror
  #-}

module HMumps.Routine(Routine,
                      Line,
                      File,
                      OldFile,
                      Tag,
                      transform,
                      pack)
 where


import Data.Map

import Prelude hiding (map)
import qualified Prelude as P

import HMumps.SyntaxTree


type Routine = Map String [Line]
type Line    = [Command]
type File    = [(Tag, Line)]
type OldFile = [(Tag, Int, Line)]
type Tag     = String

-- |After initial parsing, do a pass over each tag to handle things.
transform :: OldFile -> File
transform [] = []
transform (x:xs) = case x of
                     (tag,0,[])   -> (tag, [Nop]) : transform xs
                     (tag,0,cmds)| any isEmptyDo cmds -> (tag,replaceEmptyDos cmds xs) : transform xs
                                 | otherwise -> (tag,cmds) : transform xs
                     (_,_,_)    -> ("",[Nop]) : transform xs

isEmptyDo :: Command -> Bool
isEmptyDo (Do _ []) = True
isEmptyDo _         = False

replaceEmptyDos :: Line -> OldFile -> Line
replaceEmptyDos cmds oldlines =
   let helper :: Command -> Command
       helper (Do cond []) = Block cond tags llines
       helper cmd = cmd

       tags :: Routine
       tags = pack contents

       llines :: [Line]
       llines = P.map snd contents

       contents :: File
       contents = transform $ takeWhile (\(_,n,_) -> n >= 0) $ P.map (\(x,n+1,y) -> (x,n,y)) oldlines
   in  P.map helper cmds

pack :: File -> Routine
pack = undefined

