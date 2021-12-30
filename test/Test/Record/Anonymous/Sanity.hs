{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Record.Anonymous.Sanity (tests) where

import qualified Data.Record.Anonymous as R

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test.Record.Anonymous.Sanity" [
      testCase "HasField" test_HasField
    ]

{-------------------------------------------------------------------------------
  HasField

  These are not size tests, we don't worry about type-level sharing here.
-------------------------------------------------------------------------------}

simpleRecord :: R.Record '[ '("x", Bool), '("y", Char), '("z", ()) ]
simpleRecord =
     R.insert #x True
   $ R.insert #y 'a'
   $ R.insert #z ()
   $ R.empty

test_HasField :: Assertion
test_HasField = do
    assertEqual "get field 1" (R.get #x simpleRecord) True
    assertEqual "get field 2" (R.get #y simpleRecord) 'a'
    assertEqual "get field 3" (R.get #z simpleRecord) ()


-- TODO: Tests for type errors (missing fields, wrong field type, ..)
