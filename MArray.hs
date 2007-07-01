module MArray where

-- Copyright 2007 Antoine Latter
-- aslatter@gmail.com

import Data.Map
import MValue
import Prelude hiding (lookup,null)

data MArray = MArray (Maybe MValue) (Map MValue MArray)

-- Given an MArray and a list of subscripts, maybe
-- return the value associated with those subs.
mIndex :: Monad m => MArray -> [MValue] -> m MValue
mIndex (MArray v _map) []     = case v of
                                  Nothing -> fail "mIndex: value not set at specified index"
                                  Just mv -> return mv
mIndex (MArray _ map)  (x:xs) = do vc <- lookup x map
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
-- Likely doesn't work yet
order :: Monad m => MArray -> Bool -> [MValue] -> m MValue
order (MArray _ map) forward (mv:[]) = let (map1, map2) = split mv map in
  if forward
     then if null map2 then fail "Order: no higher indices"
          else let (k,_) = findMin map2 in return k
     else if null map1 then fail "Order: no lower indices"
          else let (k,_) = findMax map1 in return k
order (MArray _ map) forward (mv:ms) = do vc <- lookup mv map
                                          order vc forward ms