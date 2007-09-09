{-# OPTIONS
            -Wall
            -Werror
  #-}

module HMumps.Routine where


import Data.Map

import HMumps.SyntaxTree


type Routine = Map String [Line]
type Line    = [Command]
type File    = [(Tag, Line)]
type OldFile = [(Tag, Int, Line)]
type Tag     = String

-- |After initial parsing, do a pass over each tag to handle things.
transform :: OldFile -> File
transform = undefined

pack :: File -> Routine
pack = undefined

