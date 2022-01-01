{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Description of a record as it is known at compile time
module Data.Record.Anonymous.Plugin.Record (
    -- * General case
    Fields(..)
  , Field(..)
  , FieldLabel(..)
    -- ** Query
  , findField
    -- * Records of statically known shape
  , KnownRecord(..)
  , KnownField(..)
    -- ** Construction
  , allFieldsKnown
    -- ** Query
  , orderKnownFields
    -- * Parsing
  , parseRecord
  , parseFields
  , parseFieldLabel
  ) where

import Control.Monad
import Data.Foldable (asum)
import Data.Map

import qualified Data.Map as Map

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Parsing

{-------------------------------------------------------------------------------
  General case
-------------------------------------------------------------------------------}

-- TODO: Will need extension for the polymorphic case
data Fields =
    FieldsCons Field Fields
  | FieldsNil
  | FieldsVar TyVar

data Field = Field FieldLabel Type

data FieldLabel =
    FieldKnown FastString
  | FieldVar   TyVar
  deriving (Eq)

-- | Find field type by name
findField :: FastString -> Fields -> Maybe Type
findField nm = go
  where
    go :: Fields -> Maybe Type
    go (FieldsCons (Field label typ) fs)
      | label == FieldKnown nm = Just typ
      | otherwise              = go fs
    go FieldsNil     = Nothing
    go (FieldsVar _) = Nothing

{-------------------------------------------------------------------------------
  Records of statically known shape

  TODO: Move this to a module of its own (along with the basic definitions),
  and split the different kinds of constraints to modules of their own.
-------------------------------------------------------------------------------}

data KnownRecord a = KnownRecord {
      knownFields :: Map FastString a
    }
  deriving (Functor, Foldable, Traversable)

data KnownField a = KnownField {
      knownFieldName :: FastString -- repeated here for convenience
    , knownFieldType :: Type
    , knownFieldInfo :: a
    }
  deriving (Functor)

-- | Return map from field name to type, /if/ all fields are statically known
--
-- TODO: For our current 'Fields' definition, this will /always/ be the case,
-- but if we extend the parser to deal with field name variables or list
-- variables, this will no longer be the case.
allFieldsKnown :: Fields -> Maybe (KnownRecord (KnownField ()))
allFieldsKnown = go Map.empty
  where
    go :: Map FastString (KnownField ()) -> Fields -> Maybe (KnownRecord (KnownField ()))
    go acc = \case
        FieldsNil ->
          Just KnownRecord {
              knownFields = acc
            }
        FieldsCons (Field label typ) fs ->
          case label of
            FieldKnown nm ->
              go (Map.insert nm (knownField nm typ) acc) fs
            FieldVar _ ->
              Nothing
        FieldsVar _ ->
          Nothing

    knownField :: FastString -> Type -> KnownField ()
    knownField nm typ = KnownField {
          knownFieldName = nm
        , knownFieldType = typ
        , knownFieldInfo = ()
        }

-- | List all known-fields in order
--
-- The order here is determined by the alphabetical order as returned by
-- 'Map.elems' and friends. It is critical that this matches the order of the
-- fields in the vector assumed by the 'from' and 'to' methods of the
-- 'Generic' class instance.
orderKnownFields :: KnownRecord a -> [a]
orderKnownFields KnownRecord{knownFields} = Map.elems knownFields

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable Fields where
  ppr (FieldsCons f fs) = parens $
          text "FieldsCons"
      <+> ppr f
      <+> ppr fs
  ppr FieldsNil       = text "FieldsNil"
  ppr (FieldsVar var) = parens $ text "FieldsVar" <+> ppr var

instance Outputable Field where
  ppr (Field label typ) = parens $
          text "Field"
      <+> ppr label
      <+> ppr typ

instance Outputable FieldLabel where
  ppr (FieldKnown nm)  = parens $ text "FieldKnown" <+> ppr nm
  ppr (FieldVar   var) = parens $ text "FieldVar"   <+> ppr var

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

-- | Parse @Record r@
--
-- Returns the argument @r@
parseRecord :: ResolvedNames -> Type -> Maybe Type
parseRecord ResolvedNames{..} r = asum [
      do (tyRecord, tyFields) <- splitAppTy_maybe r
         tcRecord <- tyConAppTyCon_maybe tyRecord
         guard $ tcRecord == tyConRecord
         return tyFields
    ]

parseFields :: Type -> Maybe Fields
parseFields fields = asum [
      do (f, fs) <- parseCons fields
         f' <- parseField f
         (FieldsCons f') <$> parseFields fs
    , do parseNil fields
         return FieldsNil
    , do FieldsVar <$> getTyVar_maybe fields
    ]

parseField :: Type -> Maybe Field
parseField field = do
    (label, typ) <- parsePair field
    label' <- parseFieldLabel label
    return $ Field label' typ

parseFieldLabel :: Type -> Maybe FieldLabel
parseFieldLabel label = asum [
      FieldKnown <$> isStrLitTy     label
    , FieldVar   <$> getTyVar_maybe label
    ]
