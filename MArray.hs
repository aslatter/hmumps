module MArray where

-- Copyright 2007 Antoine Latter
-- aslatter@gmail.com

import Data.Map
import MValue
import Prelude hiding (lookup)

data MArray = ValueCell MValue (Map MValue MArray)
            | Empty (Map MValue MArray)


-- Given an MArray an d a list of subscripts, maybe
-- return the value associated with those subs.
mIndex :: MArray -> [MValue] -> Maybe MValue
mIndex (Empty _) []         = Nothing
mIndex (ValueCell v _) []   = Just v
mIndex (Empty map) (x:xs)   = do vc <- lookup x map
                                 mIndex vc xs
mIndex (ValueCell v map) (x:xs) = do vc <- lookup x map
                                     mIndex vc xs
