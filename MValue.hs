module MValue where

-- Copyright 2007 Antoine Latter
-- aslatter@gmail.com

import Char

-- The MUMPS value type - is transparently a string or int
-- or float.

data MValue = String String
            | Number Integer
            | Float  Float
 deriving (Show, Ord) -- The Ord derivation is BAD
                      -- but I need something for
                      -- MArray to work.

-- I think is is proper MUMPS equality.
-- I need to write some tests to be sure.
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
          meq ms@(String s) mv = ms == mString mv
          meq mv ms@(String s) = ms == mString mv


-- Cast to String
-- I'm not sure about how I handle floats.
mString :: MValue -> MValue
mString (Number n)   = String $ show n
mString (Float f)    = String $ if show f == (show . (/ 1.0)  . fromIntegral . truncate) f
                                then (show . truncate) f
                                else show f
mString x@(String _) = x


-- Cast to Number|Float
mNum :: MValue -> MValue
mNum (String [])  = Number 0
mNum (String s) = if isSpace (head s) then Number 0 else
  case (reads s :: [(Integer,String)]) of
    (i,s):[] -> Number i
    _        -> case (reads s :: [(Float,String)]) of
                  (f,s):[] -> Float f
                  _        -> Number 0
mNum x@(_) = x


-- NOTE: Sorting order (wrt insertion into an Array)
-- is DIFFERENT from "follows" order.

-- "Normal" form of a string (used for Ord)
-- If a String can be represented as a Number or a Float
-- without a loss of information, then do so. 
--
-- TODO: make this comply with 7.1.4.3 of the standard
--  wrt leading and trailing zeros
--
-- I really need to read the spec better before I do
-- $O sort-order for reals.  Also, so since $O sort order
-- is dynamic, (or at least environment dependent) I'm not
-- sure if it should really be hard-coded into the Ord
-- instance.
mNormal :: MValue -> MValue
mNormal ms@(String s) = if isSpace (head s) then ms else
    case (reads s :: [(Integer,String)]) of
      (i,[]):[] -> Number i
      _        -> case (reads s :: [(Float,String)]) of
                    (f,[]):[] -> Float f
                    _         -> ms 
mNormal x@(_) = x