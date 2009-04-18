{-# OPTIONS -Wall -Werror #-}

module Data.MArray (
               MArray,
               mEmpty,
               mIndex,
               arrayUpdate,
               order
              ) where

-- Copyright 2007 Antoine Latter
-- aslatter@gmail.com

import Data.Map
import Data.MValue
import Prelude hiding (lookup,null)
import Test.QuickCheck

data MArray = MArray (Maybe MValue) (Map MValue MArray)

-- |Returns an empty MArray
mEmpty :: MArray
mEmpty = MArray Nothing empty


lookup' :: (Monad m, Ord k) => k -> Map k v -> m v
lookup' k m = case lookup k m of
                Just a -> return a
                Nothing -> fail "Data.Map.lookup: failed!"

-- |Given an MArray and a list of subscripts, maybe
-- return the value associated with those subs.
mIndex :: Monad m => MArray -> [MValue] -> m MValue
mIndex (MArray v _map) []     = case v of
                                  Nothing -> fail "mIndex: value not set at specified index"
                                  Just mv -> return mv
mIndex (MArray _ map')  (x:xs) = do vc <- lookup' x map'
                                    mIndex vc xs

-- |Takes an array, subscripts and a value and returns the
-- updated array.
arrayUpdate :: MArray -> [MValue] -> MValue -> MArray
arrayUpdate (MArray  _ map') [] v' =  MArray (Just v') map'
arrayUpdate ma@(MArray n map') (sub:subs) v' =  MArray n map'' where
    
    map'' :: Map MValue MArray
    map'' = insert sub ma' map'

    ma' :: MArray
    ma' = arrayUpdate (nextArray sub ma) subs v'

-- Given an Array and a Subscript reurns either the next
-- array or an 'empty' array.
nextArray :: MValue -> MArray -> MArray
nextArray v (MArray _v map') = case lookup v map' of
    Nothing  -> MArray Nothing empty
    Just ma' -> ma'


-- |Returns the next highest subscript for the last
-- subscript provided.  Passing false for the bool
-- gives the next lowest, instead.
-- I'll be re-writing this soon (I hope!), including
-- the type-sugnature.
order :: Monad m => MArray -- ^The supplied array
      -> Bool -- ^Set to False to search backwards
      -> [MValue] -- ^The supplied subscript
      -> m MValue -- ^The next value for the last subscript provided
order (MArray _ map') forward (mv:[]) = let (map1, map2) = split mv map' in
  if forward
     then if null map2 then fail "Order: no higher indices"
          else let (k,_) = findMin map2 in return k
     else if null map1 then fail "Order: no lower indices"
          else let (k,_) = findMax map1 in return k
order (MArray _ map') forward (mv:ms) = do vc <- lookup' mv map'
                                           order vc forward ms
order _ _ [] = undefined -- some sort of base case.  this function is all messed up :-(

{-
instance (Arbitrary a) => Arbitrary (Maybe a) where
    arbitrary            = sized arbMaybe
        where
          arbMaybe 0 = return Nothing
          arbMaybe n = fmap Just (resize (n-1) arbitrary)
    coarbitrary Nothing  = variant 0
    coarbitrary (Just x) = variant 1 . coarbitrary x
-}

instance Arbitrary MArray where
    arbitrary = do
      n <- arbitrary
      xs <- arbitrary
      return $ MArray n (fromList xs)
    coarbitrary (MArray n xs) = variant 0 . coarbitrary (n,toList xs)