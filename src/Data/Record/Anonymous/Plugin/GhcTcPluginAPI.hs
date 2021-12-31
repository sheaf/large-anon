{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Thin layer around ghc-tcplugin-api
module Data.Record.Anonymous.Plugin.GhcTcPluginAPI (
    -- * Standard exports
    module GHC.TcPlugin.API
  , module GHC.Utils.Outputable
  , module GHC.Core.Make

    -- * Additional re-exports
    -- ** Parsing types
  , tyConAppTyCon_maybe
  , splitAppTy_maybe
  , isStrLitTy
  , Boxity(Boxed)
    -- ** Names
  , showClassName
  , typeTyConName

    -- * Bug work-arounds
  , newWanted'
  ) where

import GHC.TcPlugin.API
import GHC.TcPlugin.API.Internal (unsafeLiftTcM)
import GHC.Utils.Outputable
import GHC.Core.Make

import Type (tyConAppTyCon_maybe, splitAppTy_maybe, isStrLitTy)
import BasicTypes (Boxity(Boxed))
import GhcPlugins (MonadThings(lookupThing))
import PrelNames (showClassName)
import THNames (typeTyConName)

instance ( Monad (TcPluginM s)
         , MonadTcPlugin (TcPluginM s)
         ) => MonadThings (TcPluginM s) where
  lookupThing = unsafeLiftTcM . lookupThing

-- Orphan instance, for debugging
instance Outputable CtLoc where
  ppr _ = text "<CtLoc>"

-- | Construct new wanted constraint
--
-- Work-around bug in ghc, making sure the location is set correctly.
-- TODO: Should this live in ghc-tcplugin-api?
-- (Is it even still needed now that we use the lib, or is this a remnant
-- from the pre-lib days..?)
newWanted' :: CtLoc -> PredType -> TcPluginM 'Solve CtEvidence
newWanted' l w = setCtLocM l $ newWanted l w


