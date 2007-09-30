{-# OPTIONS
            -Wall
            -Werror
  #-}

module HMumps.Routine(--Routine,
                      Line,
                      File,
                      OldFile,
                      Routine,
                      --Tag,
                      transform,
                      pack)
 where



-- import qualified Prelude as P

import HMumps.Types

-- |After initial parsing, do a pass over each tag to handle things.
transform :: OldFile -> File
transform [] = []
transform (x:xs) = case x of
                     (tag,0,[])   -> (tag, [Nop]) : transform xs
                     (tag,0,cmds)| any isEmptyDo cmds -> (tag,replaceEmptyDos cmds xs) : transform xs
                                 | otherwise -> (tag,cmds) : transform xs
                     (_,_,_)    -> (Nothing,[Nop]) : transform xs

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
       llines = fmap snd contents

       contents :: File
       contents = transform $ takeWhile (\(_,n,_) -> n >= 0) $ fmap (\(x,n,y) -> (x,n-1,y)) oldlines
   in  fmap helper cmds

pack :: File -> Routine
pack = undefined

