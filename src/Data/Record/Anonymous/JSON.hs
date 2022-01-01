{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Record.Anonymous.JSON (
    recordToJSON
  ) where

import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Maybe (catMaybes)
import Data.Proxy
import Data.Record.Generic
import Data.Text (Text)

import qualified Data.Record.Generic.Rep as Rep
import qualified Data.Text               as Text

-- Importing the Internal module here so that we can avoid circular dependencies
-- (importing Data.Record.Anonymous will bring the ToJSON/FromJSON instances
-- in scope).
import Data.Record.Anonymous.Internal

{-------------------------------------------------------------------------------
  Generic functions

  These can be used if the defaults used in the ToJSON/FromJSON instances
  are not suitable.
-------------------------------------------------------------------------------}

recordToJSON :: forall r.
     RecordConstraints r ToJSON
  => (String -> Maybe Text) -- ^ Select/modify field names
  -> Record r -> Value
recordToJSON f =
      object
    . catMaybes
    . Rep.collapse
    . Rep.czipWith (Proxy @ToJSON) (mapKIK aux) names
    . from
  where
    names :: Rep (K String) (Record r)
    names = recordFieldNames $ metadata (Proxy @(Record r))

    aux :: ToJSON x => String -> x -> Maybe Pair
    aux nm x = (.= x) <$> f nm

recordParseJSON :: forall r.
     RecordConstraints r FromJSON
  => (String -> Text) -- ^ Modify field names
  -> Value -> Parser (Record r)
recordParseJSON f = withObject "Record" $ \obj ->
    to <$> Rep.cmapM (Proxy @FromJSON) (aux obj) names
  where
    names :: Rep (K String) (Record r)
    names = recordFieldNames $ metadata (Proxy @(Record r))

    aux :: FromJSON x => Object -> K String x -> Parser (I x)
    aux obj (K nm) = I <$> obj .: f nm

{-------------------------------------------------------------------------------
  Default instances
-------------------------------------------------------------------------------}

instance RecordConstraints r ToJSON => ToJSON (Record r) where
  toJSON = recordToJSON (Just . Text.pack)

instance RecordConstraints r FromJSON => FromJSON (Record r) where
  parseJSON = recordParseJSON Text.pack

