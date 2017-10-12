import OpenTXLog

import Data.List
import Data.Function

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

testNoDups = assertBool "no dups" (nodups [1..6])

testHasDups = assertBool "has dups" (not $ nodups [1, 2, 3, 3, 4, 5, 6])

testNoDupsNotuniq = assertEqual "can has dup" (dropDup id [1, 1, 2, 1, 1, 3]) [1, 2, 1, 3]

prop_dedup_dedup xs = dropDup id xs == dropDup id (dropDup id xs)

prop_no_dups xs = nodups (dropDup id xs)

prop_dup_group xs = dropDup id xs == map head (groupBy (on (==) id) xs)

tests = [
  testProperty "drop dup drops dups" (prop_no_dups ::[Int] -> Bool),
  testProperty "dedup idempotency" (prop_dedup_dedup ::[Int] -> Bool),
  testProperty "dedup vs. groupBy" (prop_dup_group ::[Int] -> Bool),
  testCase "no dups" testNoDups,
  testCase "has dups" testHasDups,
  testCase "allow dups" testNoDupsNotuniq
  ]

main = defaultMain tests
