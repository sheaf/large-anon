{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Constraints.RecordMetadata (
    CRecordMetadata(..)
  , parseRecordMetadata
  , evidenceRecordMetadata
  , solveRecordMetadata
  ) where

import Data.Void

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Parsing
import Data.Record.Anonymous.Plugin.Record

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parsed form of a @RecordMetadata r@ constraint
data CRecordMetadata = CRecordMetadata {
      -- | Fields of the record
      recordMetadataFields :: Fields

      -- | Type of the record (@r@)
      --
      -- This is the only type argument to @RecordMetadata@, so we don't
      -- separately record the "raw arguments|/
    , recordMetadataTypeRecord :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CRecordMetadata where
  ppr (CRecordMetadata fields typeRecord) = parens $
          text "CRecordMetadata"
      <+> ppr fields
      <+> ppr typeRecord

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseRecordMetadata ::
     ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CRecordMetadata)
parseRecordMetadata ResolvedNames{..} =
    parseConstraint clsRecordMetadata $ \case
      [r] -> do
        fields <- parseFields r
        return CRecordMetadata {
            recordMetadataFields     = fields
          , recordMetadataTypeRecord = r
          }
      _invalidNumArgs ->
        Nothing

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

evidenceRecordMetadata ::
     ResolvedNames
  -> Fields
  -> TcPluginM 'Solve EvTerm
evidenceRecordMetadata ResolvedNames{..} _fields =
    undefined

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveRecordMetadata ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CRecordMetadata
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveRecordMetadata rn@ResolvedNames{..}
                       orig
                       (L _l CRecordMetadata{..})
                     = do
    return (Nothing, [])
{-
    -- See 'solveRecordConstraints' for a discussion of 'allFieldsKnown'
    case allFieldsKnown recordMetadataFields of
      Nothing ->
        return (Nothing, [])
      Just _fields -> do
        ev <- evidenceRecordMetadata rn recordMetadataFields
        return (Just (ev, orig), [])
-}

