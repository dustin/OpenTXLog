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

prop_no_dups xs =
  nodups (dropDup id 11 xs [])
  where types = (xs :: [Int])

tests = [
  testProperty "drop dup drops dups" prop_no_dups,
  testCase "no dups" no_dups,
  testCase "has dups" has_dups
  ]

main = defaultMain tests
