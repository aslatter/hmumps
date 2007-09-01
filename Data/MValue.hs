{-# OPTIONS -Wall -fglasgow-exts #-}

-- |This module defines the basic MUMPS type: the MValue
module Data.MValue where

-- Copyright 2007 Antoine Latter
-- aslatter@gmail.com

import Char
import Data.Ratio
import qualified Data.List as L

-- The MUMPS value type - is transparently a string or int
-- or float.

-- |Implementation-wise, the MValue is a wrapper around three
-- different Haskell types: a String, an Integer, or a Double.
-- However the integer and double representation are purely
-- for convinience, as far as the standard is concerned the number
-- type is a strict subtype of the string type.
data MValue = String String
            | Number Integer
            | Float  Double
 deriving (Show)

-- |I think this is proper MUMPS equality.
-- The key thing to watch out for is that
-- 1.0 <> "1.0", in fact 1.0 == "1".  That
-- is, numeric literals should be striped down
-- to the "canonical" numeric form before being
-- represented as a string.
instance Eq MValue where
    (==) = meq
        where
          meq :: MValue -> MValue -> Bool
          -- Easy cases
          meq (String s1) (String s2) = s1 == s2
          meq (Number n1) (Number n2) = n1 == n2
          meq (Float f1)  (Float f2)  = f1 == f2
          -- Simple numeric cases
          meq (Number n)  (Float f) = f == (fromInteger n)
          meq (Float f)   (Number n) = f == (fromInteger n)
          -- Last, conversion to strings
          meq ms@(String _) mv = ms == mString mv
          meq mv ms@(String _) = ms == mString mv

-- |This instance of Ord gives proper sorting in an
-- MArray, but does NOT give proper results for the
-- MUMPS ">" an "<" operators.
instance Ord MValue where
    compare (Number i1) (Number i2) = compare i1 i2
    compare (Float  f1) (Float  f2) = compare f1 f2
    --
    compare (Number i1) (Float  f2) = compare (fromIntegral i1) f2
    compare (Float  f1) (Number i2) = compare f1 (fromIntegral i2)
    --
    compare mv1 mv2 | (isNum mv1) && (isNum mv2) = compare (mNum mv1) (mNum mv2)
    -- 
    compare (String s1) (String s2) = compare s1 s2
    compare (String s1) mv = let (String s2) = mString mv in compare s1 s2
    compare mv (String s2) = let (String s1) = mString mv in compare s1 s2

follows :: MValue -> MValue -> MValue
follows a b = boolToM $ follows' a b
 where follows' :: MValue -> MValue -> Bool
       follows' (String l) (String r) = l > r
       follows' (String l) r = let String r' = mString r
                               in l > r'
       follows' l (String r) = let String l' = mString l
                               in l' > r
       follows' l r = let String l' = mString l
                          String r' = mString r
                      in l' > r'

contains :: MValue -> MValue -> MValue
contains a b = boolToM $ contains' a b
 where contains' :: MValue -> MValue -> Bool
       contains' (String s1) (String s2) = s2 `L.isInfixOf` s1
       contains' (String s1) mv          = let String s2 = mString mv
                                           in s2 `L.isInfixOf` s1
       contains' mv (String s2)          = let String s1 = mString mv
                                           in s2 `L.isInfixOf` s1
       contains' mv1 mv2                 = contains' (mString mv1) (mString mv2)

-- |Cast to String - the returned MValue is always built with the String
-- constructor.
mString :: MValue -> MValue
mString (Number n)   = String $ show n
mString (Float f)    = String $ if f == fromIntegral (truncate f :: Integer)
                                then show (truncate f :: Integer)
                                else show f
mString x@(String _) = x


-- |Cast to number.  Leading + or - signs are interpretted as unary
-- operators.  The supplied MValue is scanned from left to right until
-- characters that can't be interpretted in a numeric context are found.
-- The resulting number is returned as an MValue.  If the supplied MValue's
-- leading charecters cannot be interpretted in a numeric context, zero is
-- returned.
mNum :: MValue -> MValue
mNum (String [])  = Number 0
mNum (String ('+':s)) = mNum $ String s
mNum (String ('-':s)) = case mNum (String s) of
                          Number 0 -> Number 0
                          Number n -> Number (- n)
                          Float  n -> Float  (- n)
                          String _ -> error "mNum should not produce an MValue contructed with \"String\""
mNum (String s) = if isSpace (head s) then Number 0 else
  case (reads s :: [(Integer,String)]) of
    (i,_):[] -> Number i
    _        -> case (reads s :: [(Double,String)]) of
                  (f,_):[] -> Float f
                  _        -> Number 0
mNum x = x

-- |Tests to see if an MValue is a number.
-- Note that a String constructed MValue can pass this test.
isNum :: MValue -> Bool
isNum (Number _) = True
isNum (Float  _) = True
isNum mv = mv == mNum mv


mNot :: MValue -> MValue
mNot = boolToM . not . mToBool

mPlus :: MValue -> MValue
mPlus = mNum

mMinus :: MValue -> MValue
mMinus x = -x

mConcat :: MValue -> MValue -> MValue
mConcat (String left) (String right) = String $ left ++ right
mConcat l@(String _) r = l `mConcat` (mString r)
mConcat l r@(String _) = (mString l) `mConcat` r
mConcat l r = (mString l) `mConcat` (mString r)

mToBool :: MValue -> Bool
mToBool s@(String _) = (mToBool . mNum) s
mToBool (Number 0)   = False
mToBool (Number _)   = True
mToBool (Float 0)    = False
mToBool (Float _)    = True

boolToM :: Bool -> MValue
boolToM True  = Number 1
boolToM False = Number 0

mAnd :: MValue -> MValue -> MValue
mAnd l r = boolToM $ (mToBool l) && (mToBool r)

mOr :: MValue -> MValue -> MValue
mOr l r  = boolToM $ (mToBool l) || (mToBool r)

mEqual :: MValue -> MValue -> MValue
mEqual l r = boolToM $ l == r

mLT :: MValue -> MValue -> MValue
mLT (String l) (String r) = boolToM $ l < r
mLT (String l) r = let String r' = mString r
                    in boolToM $ l < r'
mLT l (String r) = let String l' = mString l
                    in boolToM $ l' < r
mLT l r = let String l' = mString l
              String r' = mString r
           in boolToM $ l' < r'

mGT :: MValue -> MValue -> MValue
mGT (String l) (String r) = boolToM $ l > r
mGT (String l) r = let String r' = mString r
                    in boolToM $ l > r'
mGT l (String r) = let String l' = mString l
                    in boolToM $ l' > r
mGT l r = let String l' = mString l
              String r' = mString r
           in boolToM $ l' > r'

mNumBinop :: (forall a . Num a => a -> a -> a) -> (MValue -> MValue -> MValue)
mNumBinop op (Number a) (Number b)   = Number $ a `op` b
mNumBinop op (Float a)  (Number b)   = Float  $ a `op` (fromIntegral b)
mNumBinop op (Number a) (Float b)    = Float  $ (fromIntegral a) `op` b
mNumBinop op (Float a)  (Float b)    = Float  $ a `op` b
mNumBinop op l@(String _) r            = (mNum l) `op` r
mNumBinop op l r@(String _)            = l `op` (mNum r)

instance Num MValue where
    (+) = mNumBinop (+)
    (-) = mNumBinop (-)
    (*) = mNumBinop (*)

    negate (Number n)   = Number (-n)
    negate (Float f)    = Float  (-f)
    negate s@(String _) = negate . mNum $ s

    abs (Float f)    = Float  $ abs f
    abs (Number n)   = Number $ abs n
    abs s@(String _) = abs $ mNum s

    signum (Float f)    = Number $ (floor . signum) f
    signum (Number n)   = Number $ signum n
    signum s@(String _) = signum $ mNum s

    fromInteger = Number . fromIntegral

instance Real MValue where
    toRational s@(String _) = (toRational . mNum) s
    toRational (Number n)   = toRational n
    toRational (Float f)    = toRational f


instance Fractional MValue where
    fromRational a | denominator a == 1 = Number $ numerator a
                   | otherwise          = Float  $ fromRational a

    recip m@(Number 1) = m
    recip (Number n)   = Float $ 1/(fromIntegral n)
    recip (Float f)    = Float $ 1/f
    recip m@(String _) = recip $ mNum m

mRealFracOp :: (forall a . RealFrac a => a -> b) -> MValue -> b
mRealFracOp op m@(String _) = (op . mNum) m
mRealFracOp op (Number n)   = op . fromIntegral $ n
mRealFracOp op (Float f)    = op f

instance RealFrac MValue where
    properFraction m@(String _) = (properFraction . mNum) m
    properFraction (Number n)   = (fromIntegral n, 0)
    properFraction (Float f)    = let (a,b) = properFraction f in
                                  (a, Float b)

    truncate       = mRealFracOp truncate
    round          = mRealFracOp round
    ceiling        = mRealFracOp ceiling
    floor          = mRealFracOp floor

mFloatUnop :: (forall a . Floating a => a -> a) -> (MValue -> MValue)
mFloatUnop op (Float f)    = Float $ op f
mFloatUnop op (Number n)   = Float . op $ fromIntegral n
mFloatUnop op s@(String _) = op . mNum $ s

mFloatBinop :: (forall a . Floating a => a -> a -> a)
               -> (MValue -> MValue -> MValue)
mFloatBinop op (Float f1) (Float f2)   = Float $ f1 `op` f2
mFloatBinop op (Float f1) (Number n2)  = Float $ f1 `op` (fromIntegral n2)
mFloatBinop op (Number n1) (Float f2)  = Float $ (fromIntegral n1) `op` f2
mFloatBinop op (Number n1) (Number n2) = Float $ (fromIntegral n1) `op` (fromIntegral n2)
mFloatBinop op s@(String _) mv         = (mNum s) `op` mv
mFloatBinop op mv s@(String _)         = mv `op` (mNum s)

instance Floating MValue where
    pi = Float pi

    exp   = mFloatUnop exp
    log   = mFloatUnop log
    sqrt  = mFloatUnop sqrt
    sin   = mFloatUnop sin
    cos   = mFloatUnop cos
    tan   = mFloatUnop tan
    asin  = mFloatUnop asin
    acos  = mFloatUnop acos
    atan  = mFloatUnop atan
    sinh  = mFloatUnop sinh
    cosh  = mFloatUnop cosh
    tanh  = mFloatUnop tanh
    asinh = mFloatUnop asinh
    acosh = mFloatUnop acosh
    atanh = mFloatUnop atanh

    (**)    = mFloatBinop (**)
    logBase = mFloatBinop logBase

mQuot (Number n1) (Number n2) = Number $ quot n1 n2
mQuot (Float f1) mv   = mQuot (Number . truncate $ f1) mv
mQuot mv (Float f2)   = mQuot mv (Number . truncate $ f2)
mQuot s@(String _) mv = mQuot (mNum s) mv
mQuot mv s@(String _) = mQuot mv (mNum s)

mRem (Number n1) (Number n2) = Number $ rem n1 n2
mRem (Float f1) mv   = mRem (Number . truncate $ f1) mv
mRem mv (Float f2)   = mRem mv (Number . truncate $ f2)
mRem s@(String _) mv = mRem (mNum s) mv
mRem mv s@(String _) = mRem mv (mNum s)