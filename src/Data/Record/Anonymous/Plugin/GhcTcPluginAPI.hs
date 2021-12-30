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
  ) where

import GHC.TcPlugin.API
import GHC.TcPlugin.API.Internal (unsafeLiftTcM)
import GHC.Utils.Outputable
import GHC.Core.Make

import Type (tyConAppTyCon_maybe, splitAppTy_maybe, isStrLitTy)
import BasicTypes (Boxity(Boxed))
import GhcPlugins (MonadThings(lookupThing))
import PrelNames (showClassName)

instance ( Monad (TcPluginM s)
         , MonadTcPlugin (TcPluginM s)
         ) => MonadThings (TcPluginM s) where
  lookupThing = unsafeLiftTcM . lookupThing
