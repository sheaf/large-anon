{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.NameResolution (
    ResolvedNames(..)
  , nameResolution
  ) where

import GHC.TcPlugin.API

data ResolvedNames = ResolvedNames {
      clsHasField            :: Class
    , tyConRecord            :: TyCon
    , idUnsafeRecordHasField :: Id
    }

nameResolution :: TcPluginM 'Init ResolvedNames
nameResolution = do

    ghcRecordsCompat <-
      getModule "record-hasfield" "GHC.Records.Compat"
    dataRecordAnonymousInternal <-
      getModule "large-anon" "Data.Record.Anonymous.Internal"

    clsHasField <-
      getClass ghcRecordsCompat "HasField"
    tyConRecord <-
      getTyCon dataRecordAnonymousInternal "Record"
    idUnsafeRecordHasField <-
      getVar dataRecordAnonymousInternal "unsafeRecordHasField"

    return $ ResolvedNames {..}

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getModule :: MonadTcPlugin m => String -> String -> m Module
getModule pkg modl = do
    r <- findImportedModule (mkModuleName modl) (Just (fsLit pkg))
    case r of
      Found _ m  -> return m
      _otherwise -> panic $ "Could not find " ++ modl ++ " in package " ++ pkg

getClass :: MonadTcPlugin m => Module -> String -> m Class
getClass modl cls = lookupOrig modl (mkTcOcc cls) >>= tcLookupClass

getTyCon :: MonadTcPlugin m => Module -> String -> m TyCon
getTyCon modl con = lookupOrig modl (mkTcOcc con) >>= tcLookupTyCon

getVar :: MonadTcPlugin m => Module -> String -> m Id
getVar modl var = lookupOrig modl (mkVarOcc var) >>= tcLookupId