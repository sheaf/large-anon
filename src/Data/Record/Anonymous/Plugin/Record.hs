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

-- TODO: Will need extension for the polymorphic case
data Field =
    -- | Name and type of a known (non-polymorphic) field
    FieldKnown FastString Type

findField :: FastString -> Fields -> Maybe Type
findField nm = go
  where
    go :: Fields -> Maybe Type
    go (FieldsCons (FieldKnown nm' typ) fs)
      | nm == nm' = Just typ
      | otherwise = go fs
    go FieldsNil = Nothing

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
        FieldsCons f fs ->
          case f of
            FieldKnown nm typ ->
              go (Map.insert nm (knownField nm typ) acc) fs

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
  ppr FieldsNil = text "FieldsNil"

instance Outputable Field where
  ppr (FieldKnown nm typ) = parens $
          text "FieldKnown"
      <+> ppr nm
      <+> ppr typ

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

-- | Parse @Record r@
--
-- Returns the  argument @r@
parseRecord :: ResolvedNames -> Type -> Maybe Type
parseRecord ResolvedNames{..} r = do
    (tyRecord, tyFields) <- splitAppTy_maybe r
    tcRecord <- tyConAppTyCon_maybe tyRecord
    guard $ tcRecord == tyConRecord
    return tyFields

parseFields :: Type -> Maybe Fields
parseFields fields = asum [
      do (f, fs) <- parseCons fields
         f' <- parseField f
         (FieldsCons f') <$> parseFields fs
    , do parseNil fields
         return FieldsNil
    ]

parseField :: Type -> Maybe Field
parseField field = asum [
      do (nm, typ) <- parsePair field
         nm' <- isStrLitTy nm
         return $ FieldKnown nm' typ
    ]

