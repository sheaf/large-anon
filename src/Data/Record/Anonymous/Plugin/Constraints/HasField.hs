{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Constraints.HasField (
    CHasField(..)
  , parseHasField
  , evidenceHasField
  , solveHasField
  ) where

import Control.Monad
import Data.Void

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Parsing
import Data.Record.Anonymous.Plugin.Record

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parsed form of a @HasField x r a@ constraint
data CHasField = CHasField {
      -- | Label we're looking for (@x@)
      --
      -- This is always a monomorphic, statically known string; if we don't
      -- know what label we're looking for, we'll definitely not be able
      -- to resolve the constraint.
      hasFieldLabel :: FastString

      -- | Fields of the record
      --
      -- These may be fully or partially known, or completely unknown.
    , hasFieldRecord :: Fields

      -- | Raw arguments to @HasField@ (for evidence construction)
    , hasFieldTypeRaw :: [Type]

      -- | Type of the record (@r@)
    , hasFieldTypeRecord :: Type

      -- | Type of the record field we're looking for (@a@)
    , hasFieldTypeField :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CHasField where
  ppr (CHasField label record typeRaw typeRecord typeField) = parens $
          text "CHasField"
      <+> ppr label
      <+> ppr record
      <+> ppr typeRaw
      <+> ppr typeRecord
      <+> ppr typeField

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseHasField ::
     ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CHasField)
parseHasField rn@ResolvedNames{..} =
    parseConstraint clsHasField $ \case
      args@[k, x, r, a] -> do
        -- Check the kind
        tcSymbol <- tyConAppTyCon_maybe k
        guard $ tcSymbol == typeSymbolKindCon

        -- We insist the name we're looking for is statically known
        x' <- isStrLitTy x

        -- Check that it's of the form @Record r@
        tyFields <- parseRecord rn r
        fields   <- parseFields tyFields

        return $ CHasField {
            hasFieldLabel      = x'
          , hasFieldRecord     = fields
          , hasFieldTypeRaw    = args
          , hasFieldTypeRecord = tyFields
          , hasFieldTypeField  = a
          }
      _invalidNumArgs ->
        Nothing

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

evidenceHasField ::
     ResolvedNames
  -> CHasField
  -> TcPluginM 'Solve EvTerm
evidenceHasField ResolvedNames{..} CHasField{..} = do
    str <- mkStringExprFS hasFieldLabel
    return $
      evDataConApp
        (classDataCon clsHasField)
        hasFieldTypeRaw
        [ mkCoreApps (Var idUnsafeRecordHasField) [
              Type hasFieldTypeRecord
            , Type hasFieldTypeField
            , str
            ]
        ]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveHasField ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CHasField
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveHasField rn orig (L l hf@CHasField{..}) =
    case findField hasFieldLabel hasFieldRecord of
      Nothing ->
        -- TODO: If the record is fully known, we should issue a custom type
        -- error here rather than leaving the constraint unsolved
        return (Nothing, [])
      Just typ -> do
        eq <- newWanted' l $ mkPrimEqPredRole Nominal hasFieldTypeField typ
        ev <- evidenceHasField rn hf
        return (Just (ev, orig), [mkNonCanonical eq])
