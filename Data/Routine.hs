{-# OPTIONS
            -Wall
            -Werror
  #-}

module Data.Routine where


import Data.Map

import HMumps.SyntaxTree


type Routine = Map String [Line]
type Line    = [Command]
type File    = [(Maybe Tag, Line)]
type Tag     = String

-- |After initial parsing, do a pass over each tag to handle things.
transform :: File -> File
transform = undefined

pack :: File -> Routine
pack = undefined

