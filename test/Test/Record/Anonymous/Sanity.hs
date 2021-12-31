{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Record.Anonymous.Sanity (tests) where

import Data.Record.Anonymous.Internal (gshowRecord) -- TODO: temporary

import qualified Data.Record.Anonymous as R

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test.Record.Anonymous.Sanity" [
      testCase "HasField" test_HasField
    , testCase "Show"     test_Show
    ]

-- 1

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

    -- TODO: We should do whole-record comparisons, but for that we need
    -- Show and Eq instances, which will depend on generics

    assertEqual
      "set field 1, then get field 1"
      (R.get #x (R.set #x False simpleRecord))
      False
    assertEqual
      "set field 1, then get field 2"
      (R.get #y (R.set #x False simpleRecord))
      'a'

    -- TODO: think about and test what happens with duplicate labels

test_Show :: Assertion
test_Show = do
    putStrLn $ gshowRecord simpleRecord