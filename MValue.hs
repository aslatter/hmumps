module MValue where

-- The MUMPS value type - is transparently a string or int
-- or float.  I'm not sure what to do about this.

data MValue = String String
            | Number Int
            | Float  Float

-- instance Show MValue where
-- show (String s) = s
-- show (Number i) = show i
-- show (Float f)  = show f

-- Cast to String
mString :: MValue -> MValue
mString = String . show

-- Cast to Number
mNum :: MValue -> MValue
mNum n@(Number _) = n
mNum f@(Float  _) = f
mNum (String s) = error "Conversion from string to num not ready yet"



-- INSERT INSTANCES OF EQ, ORD, SHOW &C. HERE

