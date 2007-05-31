module MValue where

-- Copyright 2007 Antoine Latter
-- aslatter@gmail.com

import Char

-- The MUMPS value type - is transparently a string or int
-- or float.

data MValue = String String
            | Number Integer
            | Float  Float
 deriving Show


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


-- "Normal" form of a string (used for some sorting)
mNormal :: MValue -> MValue
mNormal (String s) = if isSpace (head s) then String s else
    case (reads s :: [(Integer,String)]) of
      (i,[]):[] -> Number i
      _        -> case (reads s :: [(Float,String)]) of
                    (f,[]):[] -> Float f
                    _        -> String s
mNormal x@(_) = x



-- INSERT INSTANCES OF EQ, ORD &C. HERE
-- this is gonna be the combinatorial mess
