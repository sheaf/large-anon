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

{-------------------------------------------------------------------------------
  HasField

  These are not size tests, we don't worry about type-level sharing here.
-------------------------------------------------------------------------------}

-- | The equivalent non-anonymous record (for comparison)
data Record = Record { x :: Bool, y :: Char, z :: () }
  deriving (Show)

simpleRecord :: R.Record '[ '("x", Bool), '("y", Char), '("z", ()) ]
simpleRecord =
      R.insert #x True
    $ R.insert #y 'a'
    $ R.insert #z ()
    $ R.empty

test_HasField :: Assertion
test_HasField = do
    assertEqual "get field 1" True $ (R.get #x simpleRecord)
    assertEqual "get field 2" 'a'  $ (R.get #y simpleRecord)
    assertEqual "get field 3" ()   $ (R.get #z simpleRecord)

    -- TODO: We should do whole-record comparisons, but for that we need
    -- Show and Eq instances, which will depend on generics

    assertEqual "set field 1, then get field 1" False $
      R.get #x (R.set #x False simpleRecord)
    assertEqual "set field 1, then get field 2" 'a' $
      (R.get #y (R.set #x False simpleRecord))

    -- TODO: think about and test what happens with duplicate labels

test_Show :: Assertion
test_Show = do
    -- TODO: When we use a wildcard for the expected value here, the plugin
    -- panics.
    assertEqual "" (show (Record True 'a' ())) $
      gshowRecord simpleRecord