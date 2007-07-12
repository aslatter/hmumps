-- |This module defines the basic MUMPS type: the MValue
module MValue where

-- Copyright 2007 Antoine Latter
-- aslatter@gmail.com

import Char

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
    v1 == v2 = meq v1 v2
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


-- |Cast to String - the returned MValue is always built witht the String
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
