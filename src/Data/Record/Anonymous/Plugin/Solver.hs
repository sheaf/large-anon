{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Solver (
    solve
  ) where

import Data.Bifunctor
import Data.Maybe (catMaybes)
import Data.Traversable (forM, for)

import Data.Record.Anonymous.Plugin.Constraints
import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Internal.Util (concatM)
import Data.Foldable (toList)

{-------------------------------------------------------------------------------
  Top-level solver
-------------------------------------------------------------------------------}

solve :: ResolvedNames -> TcPluginSolver
solve rn given wanted = trace _debugOutput $ do
    (solved, new) <- fmap (bimap catMaybes concat . unzip) $ concatM [
        forM parsedHasField          $ uncurry (solveHasField          rn)
      , forM parsedRecordConstraints $ uncurry (solveRecordConstraints rn)
      , forM parsedRecordMetadata    $ uncurry (solveRecordMetadata    rn)
      ]
    return $ TcPluginOk solved new
  where
    parsedHasField          :: [(Ct, GenLocated CtLoc CHasField)]
    parsedRecordConstraints :: [(Ct, GenLocated CtLoc CRecordConstraints)]
    parsedRecordMetadata    :: [(Ct, GenLocated CtLoc CRecordMetadata)]

    parsedHasField =
        parseAll' (withOrig (parseHasField rn)) wanted
    parsedRecordConstraints =
        parseAll' (withOrig (parseRecordConstraints rn)) wanted
    parsedRecordMetadata =
        parseAll' (withOrig (parseRecordMetadata rn)) wanted

    _debugOutput :: String
    _debugOutput = unlines [
          "*** solve"
        , concat [
              "given:"
            , showSDocUnsafe (ppr given)
            ]
        , concat [
              "wanted: "
            , showSDocUnsafe (ppr wanted)
            ]
        , concat [
              "parsedHasField: "
            , showSDocUnsafe (ppr parsedHasField)
            ]
        , concat [
              "parsedRecordConstraints: "
            , showSDocUnsafe (ppr parsedRecordConstraints)
            ]
        , concat [
              "parsedRecordMetadata: "
            , showSDocUnsafe (ppr parsedRecordMetadata)
            ]
        ]

{-------------------------------------------------------------------------------
  HasField
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

{-------------------------------------------------------------------------------
  RecordConstraints
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

{-------------------------------------------------------------------------------
  RecordMetadata
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

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Construct new wanted constraint
--
-- Work-around bug in ghc, making sure the location is set correctly.
-- TODO: Should this live in ghc-tcplugin-api?
-- (Is it even still needed now that we use the lib, or is this a remnant
-- from the pre-lib days..?)
newWanted' :: CtLoc -> PredType -> TcPluginM 'Solve CtEvidence
newWanted' l w = setCtLocM l $ newWanted l w

