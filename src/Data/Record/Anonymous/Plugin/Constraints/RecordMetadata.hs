{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Constraints.RecordMetadata (
    CRecordMetadata(..)
  , parseRecordMetadata
  , evidenceRecordMetadata
  , solveRecordMetadata
  ) where

import Data.Foldable (toList)
import Data.Traversable (for)
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
    parseConstraint' clsRecordMetadata $ \case
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

-- | Construct evidence
--
-- For each field we need an evidence variable corresponding to the evidence
-- that that field name satisfies KnownSymbol.
evidenceRecordMetadata ::
     ResolvedNames
  -> CRecordMetadata
  -> KnownRecord (KnownField EvVar)
  -> TcPluginM 'Solve EvTerm
evidenceRecordMetadata ResolvedNames{..}
                       CRecordMetadata{..}
                       fields@KnownRecord{..}
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
                , mkListExpr fieldMetadataType $
                    map mkFieldInfoAny (orderKnownFields fields)
                ]
            ]
        ]
  where
    fieldMetadataType :: Type
    fieldMetadataType = mkTyConApp tyConFieldMetadata [anyType]

    mkFieldInfoAny :: KnownField EvVar -> EvExpr
    mkFieldInfoAny KnownField{ knownFieldName = fieldName
                             , knownFieldInfo = dict
                             } =
        mkCoreConApps dataConFieldMetadata [
            Type anyType
          , Type (mkStrLitTy fieldName)
          , Var dict
          , mkCoreConApps dataConProxy [
                Type $ mkTyConTy typeSymbolKindCon
              , Type $ mkStrLitTy fieldName
              ]
            -- TODO: Think about strict/lazy fields
          , mkCoreConApps dataConFieldLazy []
          ]

    -- Any at kind Type
    anyType :: Type
    anyType = mkTyConApp anyTyCon [liftedTypeKind]

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
                       (L l cm@CRecordMetadata{..})
                     = do
    -- See 'solveRecordConstraints' for a discussion of 'allFieldsKnown'
    case allFieldsKnown recordMetadataFields of
      Nothing ->
        return (Nothing, [])
      Just fields -> do
        fields' <- for fields $ \field@KnownField{knownFieldName} -> do
          ev <- newWanted' l $
                  mkClassPred clsKnownSymbol [mkStrLitTy knownFieldName]
          return $ const ev <$> field
        ev <- evidenceRecordMetadata rn cm $ fmap getEvVar <$> fields'
        return (
            Just (ev, orig)
          , map (mkNonCanonical . knownFieldInfo) (toList fields')
          )
  where
    getEvVar :: CtEvidence -> EvVar
    getEvVar ct = case ctev_dest ct of
      EvVarDest var -> var
      HoleDest  _   -> error "impossible (we don't ask for primitive equality)"
