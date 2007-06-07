module Testing 
    where
    
import LineAST
import MValue
import MArray
import Test.QuickCheck

instance Arbitrary Char where
    arbitrary = elements ('%':['A'..'z'])
    coarbitrary c = variant (fromEnum c `rem` 4)

instance Arbitrary MValue where
    arbitrary = oneof [do
                         x <- arbitrary
                         return $ String x,
                       do
                         x <- arbitrary
                         return $ Number x,
                       do
                         x <- arbitrary
                         return $ Float x]
    coarbitrary (String s) = variant 0 . coarbitrary s
    coarbitrary (Number n) = variant 1 . coarbitrary n
    coarbitrary (Float f) = variant 2 . coarbitrary f