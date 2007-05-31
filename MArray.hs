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
