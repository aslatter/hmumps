module MArray where

-- Copyright 2007 Antoine Latter
-- aslatter@gmail.com

import Data.Map
import MValue
import Prelude hiding (lookup)

type MArray = (Maybe MValue, Map MValue MArray)

-- Given an MArray an d a list of subscripts, maybe
-- return the value associated with those subs.
mIndex :: MArray -> [MValue] -> Maybe MValue
mIndex (v, _map) []     = v
mIndex (v, map)  (x:xs) = do vc <- lookup x map
                             mIndex vc xs

firstKey :: MArray -> Maybe MValue
firstKey ma = case toList ma of
                []      -> Nothing
                (v,k):_ -> Just k

lastKey :: MArray -> Maybe MValue -- Slower than firstKey :-(
lastKey ma = case reverse toList ma of
               []      -> Nothing
               (v,k):_ -> Just k

-- Returns the next highest subscript for the last
-- subscript provided.  Passing false for the bool
-- gives the next lowest, instead.
order :: MArray -> Bool -> [MValue] -> Maybe MValue
-- Forward search
order (_,map) forward (mv:[]) = let (map1, map2) = split mv map in
  if forward
     then firstKey map2
     else lastKey  map1
order (_,map) forward (mv:ms) = do vc <- lookup mv map
                                   order vc forward ms

-- Given an array and subscripts, returns the "next" set
-- of subscripts.  The spec is a bit hard to read on this.
query :: MArray -> [MValue] -> [MValue]
query vc ms = iter [vc] [] ms
 where
   iter (vc:vcs) acc (m:[]) = case order vc True [m] of
     Just m' -> reverse (m':acc)
     Nothing -> case mIndex vc [m] of
       Just vc' -> case firstKey vc' of
         Just k   -> reverse $ k : acc
         Nothing  -> iter vcs (tail acc) (head acc) -- I'm assuming re:acc
     Nothing -> iter vcs (tail acc) (head acc) -- again assuming re:acc
   iter (vc:vcs) acc (m:ms) = case mIndex vc [m] of
     Just vc' -> iter (vc':vc:vcs) (m:acc) ms