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
    assertEqual "get first field" (R.get #x simpleRecord) True
    assertEqual "get first field" (R.get #y simpleRecord) 'y'
    assertEqual "get first field" (R.get #z simpleRecord) ()

-- TODO: Tests for type errors (missing fields, wrong field type, ..)
