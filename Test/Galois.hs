module Test.Galois where

import Galois
import Test.QuickCheck

instance Arbitrary GF where
  arbitrary = do
    n <- arbitrary
    return $ GF n

prop_mult_commute :: GF -> GF -> Bool
prop_mult_commute a b = a * b == b * a

prop_1_mult_ident :: GF -> Bool
prop_1_mult_ident a = a * 1 == a

prop_0_mult_unit a = a * 0 == 0

prop_distrib :: GF -> GF -> GF -> Bool
prop_distrib a b c = a * (b+c) == (a*b) + (a*c)

prop_unique :: GF -> GF -> GF -> Bool
prop_unique a b c = a == 0 || b == 0 || c == 0 || a == b || a == c || b == c || (a*b /= a*c && b*c /= a*c && b*c /= b*a)
