{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import OpenTXLog

import Data.List (groupBy)
import Data.Function (on)
import Data.Csv (encode)

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Golden
import Test.Invariant (idempotent, (<=>), (&>))

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
prop_dup_group = dropDup id <=> map head . groupBy (on (==) id)

testFieldTransformer :: [TestTree]
testFieldTransformer = map (\(t, (h,r,n,f), want) -> testCase t $ assertEqual t (fieldTransformer n f h r) want) [
  ("empty", ([], [], "field", const "newval"), []),
  ("nomatch", (["a", "b"], ["1", "2"], "x", const "newval"), ["1", "2"]),
  ("first", (["a", "b"], ["1", "2"], "a", const "newval"), ["newval", "2"]),
  ("second", (["a", "b"], ["1", "2"], "b", const "newval"), ["1", "newval"])
  ]

testIntFieldTransformer :: [TestTree]
testIntFieldTransformer = map (\(t, (h,r,n,f), want) -> testCase t $ assertEqual t (intFieldTransformer n f h r) want) [
  ("empty", ([], [], "field", const 0), []),
  ("nomatch", (["a", "b"], ["1", "2"], "x", succ), ["1", "2"]),
  ("first", (["a", "b"], ["1", "2"], "a", succ), ["2", "2"]),
  ("second", (["a", "b"], ["1", "2"], "b", succ), ["1", "3"])
  ]

tests :: [TestTree]
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

  testGroup "field transformer" testFieldTransformer,
  testGroup "int field transformer" testIntFieldTransformer,

  testCase "no dups" testNoDups,
  testCase "has dups" testHasDups,
  testCase "allow dups" testNoDupsNotuniq,

  goldenVsStringDiff "golden sample" (\ref new -> ["diff", "-u", ref, new])
    "test/sample.out.csv" (pfile "test/sample.csv")
  ]

  where pfile file = encode <$> processCSVFile file []

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
