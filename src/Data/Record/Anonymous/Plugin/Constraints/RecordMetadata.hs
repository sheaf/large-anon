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

import qualified Data.Map as Map

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
  -> CRecordMetadata
  -> KnownRecord (KnownField ())
  -> TcPluginM 'Solve EvTerm
evidenceRecordMetadata ResolvedNames{..}
                       CRecordMetadata{..}
                       KnownRecord{..}
                     = do
    nameRecord <- mkStringExpr "Record"
    nameConstr <- mkStringExpr "Record"
    return $
      evDataConApp
        (classDataCon clsRecordMetadata)
        [recordMetadataTypeRecord]
        [ mkCoreConApps dataConMetadata [
              Type $ mkTyConApp tyConRecord [recordMetadataTypeRecord]
            , nameRecord
            , nameConstr
            , mkUncheckedIntExpr (fromIntegral (Map.size knownFields))
            , mkCoreApps (Var idUnsafeFieldMetadata) [
                  Type recordMetadataTypeRecord
                ]
            ]
        ]

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
                       (L _l cm@CRecordMetadata{..})
                     = do
    -- See 'solveRecordConstraints' for a discussion of 'allFieldsKnown'
    case allFieldsKnown recordMetadataFields of
      Nothing ->
        return (Nothing, [])
      Just fields -> do
        ev <- evidenceRecordMetadata rn cm fields
        return (Just (ev, orig), [])
