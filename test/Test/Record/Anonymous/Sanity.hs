{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}
{-# OPTIONS_GHC -fno-show-valid-hole-fits #-}

module Test.Record.Anonymous.Sanity (tests) where

import Data.Aeson

import qualified Data.Record.Anonymous as R

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test.Record.Anonymous.Sanity" [
      testCase "HasField"       test_HasField
    , testCase "Show"           test_Show
    , testCase "Eq"             test_Eq
    , testCase "Ord"            test_Ord
    , testCase "describeRecord" test_describeRecord
    , testCase "JSON"           test_JSON
    ]

{-------------------------------------------------------------------------------
  HasField

  These are not size tests, we don't worry about type-level sharing here.
-------------------------------------------------------------------------------}

-- | The equivalent non-anonymous record (for comparison)
data Record = Record { x :: Bool, y :: Char, z :: () }
  deriving (Show, Eq, Ord)

record1 :: R.Record '[ '("x", Bool), '("y", Char), '("z", ()) ]
record1 =
      R.insert #x True
    $ R.insert #y 'a'
    $ R.insert #z ()
    $ R.empty

-- | Second example, where the fields do not appear in alphabetical order
--
-- Ordering matters in the 'Generic' instance.
record2 :: R.Record '[ '("y", Char), '("x", Bool) ]
record2 =
      R.insert #y 'a'
    $ R.insert #x True
    $ R.empty

test_HasField :: Assertion
test_HasField = do
    assertEqual "get field 1" True $ (R.get #x record1)
    assertEqual "get field 2" 'a'  $ (R.get #y record1)
    assertEqual "get field 3" ()   $ (R.get #z record1)

    -- TODO: We should do whole-record comparisons, but for that we need
    -- Show and Eq instances, which will depend on generics

    assertEqual "set field 1, then get field 1" False $
      R.get #x (R.set #x False record1)
    assertEqual "set field 1, then get field 2" 'a' $
      (R.get #y (R.set #x False record1))

    -- TODO: think about and test what happens with duplicate labels

test_Show :: Assertion
test_Show = do
    assertEqual "" (show (Record True 'a' ())) $ show record1

test_Eq :: Assertion
test_Eq = do
    assertEqual "equal" True $
      record1 == record1
    assertEqual "not equal" False $
      record1 == (R.set #x False record1)

test_Ord :: Assertion
test_Ord = do
    assertEqual "" (compare (Record True 'a' ()) (Record False 'a' ())) $
      compare record1 (R.set #x False record1)

-- Test 'describeRecord'
--
-- The primary motivation for this test is actually not the function itself,
-- but to verify that constraint resolution is working ok. Specifically,
-- that the implicit kind argument to 'Typeable' is handled by ghc and does not
-- need to be taken into account by the @large-anon@ plugin.
test_describeRecord :: Assertion
test_describeRecord = do
    assertEqual "" expected $ R.describeRecord record1
  where
    expected :: String
    expected = "Record {x :: Bool, y :: Char, z :: ()}"

test_JSON :: Assertion
test_JSON = do
    print record2
    print $ encode record1
    print $ (decode (encode record1) :: Maybe (R.Record '[ '("x", Bool), '("y", Char), '("z", ()) ] ))
    print $ encode record2
    print $ (decode (encode record2) :: Maybe (R.Record '[ '("y", Char), '("x", Bool) ] ))