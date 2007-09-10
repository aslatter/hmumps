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

import HMumps.SyntaxTree


type Routine = Map String [Line]
type Line    = [Command]
type File    = [(Tag, Line)]
type OldFile = [(Tag, Int, Line)]
type Tag     = String

-- |After initial parsing, do a pass over each tag to handle things.
transform :: OldFile -> File
transform xs = transform' [] xs

transform' :: File -> OldFile -> File
transform' fs [] = reverse fs
transform' fs (x:xs) = let f' = case x of
                                 (tag,0,[])   -> (tag, [Nop])

                                 (tag,0,cmds)| hasFor cmds -> undefined tag
                                             | hasDo  cmds -> undefined tag

                                 (_,_,_)    -> ("",[Nop])


                       in transform' (f':fs) xs

hasFor :: Line -> Bool
hasFor = undefined

hasDo :: Line -> Bool
hasDo = undefined

pack :: File -> Routine
pack = undefined

