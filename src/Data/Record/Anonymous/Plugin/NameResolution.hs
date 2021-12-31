{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.NameResolution (
    ResolvedNames(..)
  , nameResolution
  ) where

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI

-- | Names we need to parse constraints or generate core
--
-- Listed alphabetically.
data ResolvedNames = ResolvedNames {
      clsHasField            :: Class
    , clsRecordConstraints   :: Class
    , clsRecordMetadata      :: Class
    , clsShow                :: Class
    , dataConDict            :: DataCon
    , idUnsafeCoerce         :: Id
    , idUnsafeDictRecord     :: Id
    , idUnsafeRecordHasField :: Id
    , tyConDict              :: TyCon
    , tyConRecord            :: TyCon
    }

nameResolution :: TcPluginM 'Init ResolvedNames
nameResolution = do

    ghcRecordsCompat <-
      getModule "record-hasfield" "GHC.Records.Compat"
    dataRecordAnonymousInternal <-
      getModule "large-anon" "Data.Record.Anonymous.Internal"
    dataSOPDict <-
      getModule "sop-core" "Data.SOP.Dict"
    unsafeCoerce <-
      getModule "base" "Unsafe.Coerce"

    clsHasField <-
      getClass ghcRecordsCompat "HasField"
    clsRecordConstraints <-
      getClass dataRecordAnonymousInternal "RecordConstraints"
    clsRecordMetadata <-
      getClass dataRecordAnonymousInternal "RecordMetadata"
    clsShow <-
      tcLookupClass showClassName

    dataConDict <-
      getDataCon dataSOPDict "Dict"

    idUnsafeCoerce <-
      getVar unsafeCoerce "unsafeCoerce"
    idUnsafeRecordHasField <-
      getVar dataRecordAnonymousInternal "unsafeRecordHasField"
    idUnsafeDictRecord <-
      getVar dataRecordAnonymousInternal "unsafeDictRecord"

    tyConDict <-
      getTyCon dataSOPDict "Dict"
    tyConRecord <-
      getTyCon dataRecordAnonymousInternal "Record"

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

getDataCon :: MonadTcPlugin m => Module -> String -> m DataCon
getDataCon modl con = lookupOrig modl (mkDataOcc con) >>= tcLookupDataCon

getVar :: MonadTcPlugin m => Module -> String -> m Id
getVar modl var = lookupOrig modl (mkVarOcc var) >>= tcLookupId