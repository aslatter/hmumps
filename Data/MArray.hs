{-# OPTIONS -Wall -Werror #-}

module Data.MArray (
               MArray,
               mEmpty,
               mIndex,
               mSubArray,
               arrayUpdate,
               killSub,
               order
              ) where

-- Copyright 2007, 2010 Antoine Latter
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
mIndex (MArray _ map')  (x:xs) = do
  vc <- lookup' x map'
  mIndex vc xs

mSubArray :: Monad m => MArray -> [MValue] -> m MArray
mSubArray m [] = return m
mSubArray (MArray _ map') (x:xs) = do
  vc <- lookup' x map'
  mSubArray vc xs

-- |Takes an array, subscripts and a value and returns the
-- updated array.
arrayUpdate :: MArray -> [MValue] -> MValue -> MArray
arrayUpdate (MArray  _ map') [] v' =  MArray (Just v') map'
arrayUpdate ma@(MArray n map') (sub:subs) v' =  MArray n map'' where
    
    map'' :: Map MValue MArray
    map'' = insert sub ma' map'

    ma' :: MArray
    ma' = arrayUpdate (nextArray sub ma) subs v'

killSub :: MArray -> [MValue] -> MArray
killSub MArray{} [] = error "fatal error in MArray.killSub"
killSub (MArray v m) [x] = MArray v $ x `delete` m
killSub a@(MArray v m) (x:xs)
    = case x `lookup` m of
        Nothing -> a
        Just a' -> MArray v $ insert x (killSub a' xs) m


-- Given an Array and a Subscript reurns either the next
-- array or an 'empty' array.
nextArray :: MValue -> MArray -> MArray
nextArray v (MArray _v map') = case lookup v map' of
    Nothing  -> MArray Nothing empty
    Just ma' -> ma'


-- |Returns the next highest subscript for the last
-- subscript provided.  Passing false for the bool
-- gives the next lowest, instead.
order :: MArray -> Bool -> MValue -> Maybe MValue
order (MArray _ map') forward mv =
    let (mapBack, mapForward) = split mv map'

        map'' | forward = mapForward
              | otherwise = mapBack

        findElem | forward = findMin
                 | otherwise = findMax

    in if null map'' then Nothing
       else let (k, _) = findElem map'' in Just k 

instance Arbitrary MArray where
    arbitrary = do
      n <- arbitrary
      xs <- arbitrary
      return $ MArray n (fromList xs)
    coarbitrary (MArray n xs) = variant 0 . coarbitrary (n,toList xs)