{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Solver (
    solve
  ) where

import Data.Maybe (catMaybes)
import Data.Traversable (forM)

import Data.Record.Anonymous.Plugin.Constraints
import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution

solve :: ResolvedNames -> TcPluginSolver
solve rn given wanted = {- trace debugOutput $ -} do
    (solved, new) <- fmap (unzip . catMaybes) $
      forM parsedHasField $ uncurry (solveHasField rn)
    return $ TcPluginOk solved new
  where
    parsedHasField :: [(Ct, GenLocated CtLoc CHasField)]
    parsedHasField = parseAll' (withOrig (parseHasField rn)) wanted

    _debugOutput :: String
    _debugOutput = unlines [
          "*** solve"
        , "given:          " ++ showSDocUnsafe (ppr given)
        , "wanted:         " ++ showSDocUnsafe (ppr wanted)
        , "parsedHasField: " ++ showSDocUnsafe (ppr parsedHasField)
        ]

solveHasField ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CHasField
  -> TcPluginM 'Solve (Maybe ((EvTerm, Ct), Ct))
solveHasField rn orig (L l hf@CHasField{..}) =
    case findField hasFieldLabel hasFieldRecord of
      Nothing ->
        -- TODO: If the record is fully known, we should issue a custom type
        -- error here rather than leaving the constraint unsolved
        return Nothing
      Just typ -> do
        eq <- newWanted' l $ mkPrimEqPredRole Nominal hasFieldTypeField typ
        ev <- evidenceHasField rn hf
        return $ Just ((ev, orig), mkNonCanonical eq)

-- Construct new wanted constraint
--
-- Work-around bug in ghc, making sure the location is set correctly.
-- TODO: Should this live in ghc-tcplugin-api?
-- (Is it even still needed now that we use the lib, or is this a remnant
-- from the pre-lib days..?)
newWanted' :: CtLoc -> PredType -> TcPluginM 'Solve CtEvidence
newWanted' l w = setCtLocM l $ newWanted l w

