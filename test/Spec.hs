import OpenTXLog

import Data.List (groupBy)
import Data.Function (on)

import Test.HUnit (Assertion, assertEqual, assertBool)
import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (a:b:xs)
  | a == b = False
  | otherwise = nodups (b:xs)
nodups _ = True

testNoDups :: Assertion
testNoDups = assertBool "no dups" (nodups [1..6])

testHasDups :: Assertion
testHasDups = assertBool "has dups" (not $ nodups [1, 2, 3, 3, 4, 5, 6])

testNoDupsNotuniq :: Assertion
testNoDupsNotuniq = assertEqual "can has dup" (dropDup id [1, 1, 2, 1, 1, 3]) [1, 2, 1, 3]

prop_dedup_dedup :: Eq a => [a] -> Bool
prop_dedup_dedup xs = dropDup id xs == dropDup id (dropDup id xs)

prop_no_dups :: Eq a => [a] -> Bool
prop_no_dups xs = nodups (dropDup id xs)

prop_dup_group :: Eq a => [a] -> Bool
prop_dup_group xs = dropDup id xs == map head (groupBy (on (==) id) xs)

tests :: [Test]
tests = [
  testProperty "drop dup drops dups" (prop_no_dups ::[Int] -> Bool),
  testProperty "dedup idempotency" (prop_dedup_dedup ::[Int] -> Bool),
  testProperty "dedup vs. groupBy" (prop_dup_group ::[Int] -> Bool),
  testCase "no dups" testNoDups,
  testCase "has dups" testHasDups,
  testCase "allow dups" testNoDupsNotuniq
  ]

main :: IO ()
main = defaultMain tests
