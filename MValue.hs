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

instance MValue Eq where
    (v1 == v2) = (meq (mNormal v1) (mNormal v2))
        where
          meq :: MValue -> MValue -> Bool
          -- Easy cases
          meq (String s1) (String s2) = s1 == s2
          meq (Number n1) (Number n2) = n1 == n2
          meq (Float f1)  (Float f2)  = f1 == f2
          -- Simple numeric cases
          meq (Number n)  (Float f) = f == (fromInteger n)
          meq (Float f)   (Number n) = f == (fromInteger n)
          -- All that's left is false
          meq (String _) (Float  _) = False
          meq (String _) (Number _) = False
          meq (Float  _) (String _) = False
          meq (Number _) (String _) = False
          -- The above should catch everything

-- Cast to String
mString :: MValue -> MValue
mString (Number n)   = String $ show n
mString (Float f)    = String $ show f
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


-- "Normal" form of a string (used for Ord and Eq)
-- If a String can be represented as a Number or a Float
-- without a loss of information, then do so. 
--
-- TODO: make this comply with 7.1.4.3 of the standard
--  wrt leading and trailing zeros
mNormal :: MValue -> MValue
mNormal ms@(String s) = if isSpace (head s) then ms else
    case (reads s :: [(Integer,String)]) of
      (i,[]):[] -> Number i
      _        -> case (reads s :: [(Float,String)]) of
                    (f,[]):[] -> Float f
                    _         -> ms 
mNormal x@(_) = x