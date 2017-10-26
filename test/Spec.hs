import OpenTXLog

import Data.List (groupBy)
import Data.Function (on)

import Test.HUnit (Assertion, assertEqual, assertBool)
import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck (Property, (==>))
import Test.Invariant (idempotent, (<=>), (&>))
import Test.Framework.Providers.QuickCheck2 (testProperty)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (a:b:xs)
  | a == b = False
  | otherwise = nodups (b:xs)
nodups _ = True

hasdups :: Eq a => [a] -> Bool
hasdups = not . nodups

testNoDups :: Assertion
testNoDups = assertBool "no dups" (nodups [1..6])

testHasDups :: Assertion
testHasDups = assertBool "has dups" (not $ nodups [1, 2, 3, 3, 4, 5, 6])

testNoDupsNotuniq :: Assertion
testNoDupsNotuniq = assertEqual "can has dup" (dropDup id [1, 1, 2, 1, 1, 3]) [1, 2, 1, 3]

prop_dedup_dedup :: [Int] -> Bool
prop_dedup_dedup = idempotent (dropDup id)

prop_no_dups :: [Int] -> Bool
prop_no_dups xs = nodups (dropDup id xs)

prop_dup_group :: [Int] -> Bool
prop_dup_group = dropDup id <=> (map head) . groupBy (on (==) id)

tests :: [Test]
tests = [
  testProperty "drop dup drops dups" prop_no_dups,
  testProperty "dedup idempotency" prop_dedup_dedup,
  testProperty "dedup vs. groupBy" prop_dup_group,

  testProperty "drop dup drops dups (all dups)" (hasdups &> prop_no_dups),
  testProperty "dedup idempotency (all dups)" (hasdups &> prop_dedup_dedup),
  testProperty "dedup vs. groupBy (all dups)" (hasdups &> prop_dup_group),

  testProperty "drop dup drops dups (no dups)" (nodups &> prop_no_dups),
  testProperty "dedup idempotency (no dups)" (nodups &> prop_dedup_dedup),
  testProperty "dedup vs. groupBy (no dups)" (nodups &> prop_dup_group),

  testCase "no dups" testNoDups,
  testCase "has dups" testHasDups,
  testCase "allow dups" testNoDupsNotuniq
  ]

main :: IO ()
main = defaultMain tests
