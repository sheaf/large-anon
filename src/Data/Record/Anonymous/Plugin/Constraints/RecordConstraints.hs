{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Constraints.RecordConstraints (
    CRecordConstraints(..)
  , parseRecordConstraints
  , evidenceRecordConstraints
  , solveRecordConstraints
  ) where

import Data.Foldable (toList)
import Data.Traversable (for)
import Data.Void

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Parsing
import Data.Record.Anonymous.Plugin.Record

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parsed form of a @RecordConstraints r c@ constraint
data CRecordConstraints = CRecordConstraints {
      -- | Fields of the record
      recordConstraintsFields :: Fields

      -- | Raw arguments to @RecordConstraints@ (for evidence construction)
    , recordConstraintsTypeRaw :: [Type]

      -- | Type of the record (@r@)
    , recordConstraintsTypeRecord :: Type

      -- | Cconstraint that we need for every field (@c@)
    , recordConstraintsTypeConstraint :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CRecordConstraints where
  ppr (CRecordConstraints fields typeRaw typeRecord typeConstraint) = parens $
          text "CRecordConstraints"
      <+> ppr fields
      <+> ppr typeRaw
      <+> ppr typeRecord
      <+> ppr typeConstraint

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseRecordConstraints ::
     ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CRecordConstraints)
parseRecordConstraints ResolvedNames{..} =
    parseConstraint clsRecordConstraints $ \case
      args@[r, c] -> do
        fields <- parseFields r
        return CRecordConstraints {
            recordConstraintsFields         = fields
          , recordConstraintsTypeRaw        = args
          , recordConstraintsTypeRecord     = r
          , recordConstraintsTypeConstraint = c
          }
      _invalidNumArgs ->
        Nothing

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

-- | Construct evidence
--
-- For each field we need an evidence variable corresponding to the evidence
-- that that fields satisfies the constraint.
evidenceRecordConstraints ::
     ResolvedNames
  -> CRecordConstraints
  -> KnownRecord (KnownField EvVar)
  -> TcPluginM 'Solve EvTerm
evidenceRecordConstraints ResolvedNames{..} CRecordConstraints{..} fields = do
    return $
      evDataConApp
        (classDataCon clsRecordConstraints)
        recordConstraintsTypeRaw
        [ mkCoreApps (Var idUnsafeDictRecord) [
              Type recordConstraintsTypeRecord
            , Type recordConstraintsTypeConstraint
            , mkListExpr dictType $
                map mkDictAny (orderKnownFields fields)
            ]
        ]
  where
    dictType :: Type
    dictType = mkTyConApp tyConDict [
          liftedTypeKind
        , recordConstraintsTypeConstraint
        , anyType
        ]

    mkDictAny :: KnownField EvVar -> EvExpr
    mkDictAny KnownField{ knownFieldType = fieldType
                        , knownFieldInfo = dict
                        } =
        mkCoreConApps dataConDict [
            Type liftedTypeKind
          , Type recordConstraintsTypeConstraint
          , Type anyType
          , mkCoreApps (Var idUnsafeCoerce) [
                Type $ mkAppTy recordConstraintsTypeConstraint fieldType
              , Type $ mkAppTy recordConstraintsTypeConstraint anyType
              , Var dict
              ]
          ]

    -- Any at kind Type
    anyType :: Type
    anyType = mkTyConApp anyTyCon [liftedTypeKind]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveRecordConstraints ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CRecordConstraints
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveRecordConstraints rn@ResolvedNames{..}
                       orig
                       (L l cr@CRecordConstraints{..})
                     = do
    -- The call to 'allFieldsKnown' establishes two things:
    --
    -- o Unless all fields of the record are known, we cannot construct the
    --   appropriate dictionary.
    -- o Moreover, that dictionary is a vector of dictionaries, one per field,
    --   and the order is determined by the field names (see also the 'Generic'
    --   instance for 'Record'). The 'Map' constructed by 'allFieldsKnown'
    --   gives us this ordering.
    case allFieldsKnown recordConstraintsFields of
      Nothing ->
        return (Nothing, [])
      Just fields -> do
        fields' <- for fields $ \field -> do
          ev <- newWanted' l $
                  mkClassPred
                    -- TODO: should be recordConstraintsTypeConstraint
                    -- (but then we need to parse more carefully)
                    clsShow
                    [knownFieldType field]
          return $ const ev <$> field
        ev <- evidenceRecordConstraints rn cr $ fmap getEvVar <$> fields'
        return (
            Just (ev, orig)
          , map (mkNonCanonical . knownFieldInfo) (toList fields')
          )
  where
    getEvVar :: CtEvidence -> EvVar
    getEvVar ct = case ctev_dest ct of
      EvVarDest var -> var
      HoleDest  _   -> error "impossible (we don't ask for primitive equality)"

