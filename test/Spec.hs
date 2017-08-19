import OpenTXLog

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

nodups [] = True
nodups (a:b:xs)
  | a == b = False
  | otherwise = nodups (b:xs)
nodups (x:xs) = True

no_dups = assertBool "no dups" (nodups [1..6])

has_dups = assertBool "has dups" (not $ nodups [1, 2, 3, 3, 4, 5, 6])

nodups_notuniq = assertEqual "can has dup" (dropDup id [1, 1, 2, 1, 1, 3]) [1, 2, 1, 3]

prop_no_dups xs =
  nodups (dropDup id xs)
  where types = (xs :: [Int])

tests = [
  testProperty "drop dup drops dups" prop_no_dups,
  testCase "no dups" no_dups,
  testCase "has dups" has_dups,
  testCase "allow dups" nodups_notuniq
  ]

main = defaultMain tests
