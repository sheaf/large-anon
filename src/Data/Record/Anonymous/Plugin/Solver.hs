{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Solver (
    solve
  ) where

import Data.Maybe (catMaybes)
import Data.Traversable (forM)

import GHC.TcPlugin.API
import GHC.Utils.Outputable

import Data.Record.Anonymous.Plugin.Constraints
import Data.Record.Anonymous.Plugin.NameResolution

solve :: ResolvedNames -> TcPluginSolver
solve rn given wanted = trace debugOutput $ do
    (solved, new) <- fmap (unzip . catMaybes) $
      forM parsedHasField $ uncurry solveHasField
    return $ TcPluginOk solved new
  where
    parsedHasField :: [(Ct, GenLocated CtLoc CHasField)]
    parsedHasField = parseAll' (withOrig (parseHasField rn)) wanted

    debugOutput :: String
    debugOutput = unlines [
          "*** solve"
        , "given:          " ++ showSDocUnsafe (ppr given)
        , "wanted:         " ++ showSDocUnsafe (ppr wanted)
        , "parsedHasField: " ++ showSDocUnsafe (ppr parsedHasField)
        ]

solveHasField ::
     Ct
  -> GenLocated CtLoc CHasField
  -> TcPluginM 'Solve (Maybe ((EvTerm, Ct), Ct))
solveHasField orig (L l CHasField{..}) =
    case findField hasFieldLabel hasFieldRecord of
      Nothing ->
        -- TODO: If the record is fully known, we should issue a custom type
        -- error here rather than leaving the constraint unsolved
        return Nothing
      Just typ -> do
        eq <- newWanted' l $ mkPrimEqPredRole Nominal hasFieldType typ
        return $ Just ((undefined, orig), mkNonCanonical eq)

-- Construct new wnated constraint
--
-- Work-around bug in ghc, making sure the location is set correctly.
-- TODO: Should this live in ghc-tcplugin-api?
-- (Is it even still needed now that we use the lib, or is this a remnant
-- from the pre-lib days..?)
newWanted' :: CtLoc -> PredType -> TcPluginM 'Solve CtEvidence
newWanted' l w = setCtLocM l $ newWanted l w

