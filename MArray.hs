module MArray where

-- Copyright 2007 Antoine Latter
-- aslatter@gmail.com

import Data.Map
import MValue
import Prelude hiding (lookup,null)

data MArray = MArray (Maybe MValue) (Map MValue MArray)

-- Given an MArray an d a list of subscripts, maybe
-- return the value associated with those subs.
mIndex :: MArray -> [MValue] -> Maybe MValue
mIndex (MArray v _map) []     = v
mIndex (MArray v  map)  (x:xs) = do vc <- lookup x map
                                    mIndex vc xs

-- Takes an array, subscripts and a value and returns the
-- updated array.
arrayUpdate :: MArray -> [MValue] -> MValue -> MArray
arrayUpdate (MArray  v map) [] v' =  MArray (Just v') map
arrayUpdate ma@(MArray _ map) (sub:subs) v' =  MArray (Just v') map' where
    
    map' :: Map MValue MArray
    map' = insert sub ma' map

    ma' :: MArray
    ma' = arrayUpdate (nextArray sub ma) subs v'

-- Given an Array and a Subscript reurns either the next
-- array or an 'empty' array.
nextArray :: MValue -> MArray -> MArray
nextArray v (MArray _v map) = case lookup v map of
    Nothing  -> MArray Nothing empty
    Just ma' -> ma'


-- Returns the next highest subscript for the last
-- subscript provided.  Passing false for the bool
-- gives the next lowest, instead.
order :: MArray -> Bool -> [MValue] -> Maybe MValue
-- Forward search
order (MArray _ map) forward (mv:[]) = let (map1, map2) = split mv map in
  if forward
     then if null map2 then Nothing
          else let (k,_) = findMin map2 in Just k
     else if null map1 then Nothing
          else let (k,_) = findMax map1 in Just k
order (MArray _ map) forward (mv:ms) = do vc <- lookup mv map
                                          order vc forward ms


-- Given an array and subscripts, returns the "next" set
-- of subscripts.  The spec is a bit hard to read on this.
--query :: MArray -> [MValue] -> [MValue]
--query vc ms = iter [vc] [] ms
-- where
--   iter (vc:vcs) acc (m:[]) = case order vc True [m] of
--     Just m' -> reverse (m':acc)
--     Nothing -> case mIndex vc [m] of
--       Just vc' -> case firstKey vc' of
--         Just k   -> reverse $ k : acc
--         Nothing  -> iter vcs (tail acc) (head acc) -- I'm assuming re:acc
--     Nothing -> iter vcs (tail acc) (head acc) -- again assuming re:acc
--   iter (vc:vcs) acc (m:ms) = case mIndex vc [m] of
--     Just vc' -> iter (vc':vc:vcs) (m:acc) ms