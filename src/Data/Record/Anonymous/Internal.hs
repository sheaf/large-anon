{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Record.Anonymous.Internal (
    -- * Types
    Record(..)
  , Field(..)
    -- * User-visible API
  , empty
  , insert
    -- * Generics
  , RecordConstraints(..)
  , RecordMetadata(..)
    -- ** Generic functions
  , gshowRecord
    -- * Internal API
  , unsafeRecordHasField
  , unsafeDictRecord
  , unsafeFieldMetadata
  ) where

import Data.Coerce (coerce)
import Data.Kind
import Data.List (intercalate)
import Data.Map (Map)
import Data.Proxy
import GHC.Exts (Any)
import GHC.OverloadedLabels
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Map as Map

import Data.Record.Generic

import qualified Data.Record.Generic.Rep          as Rep
import qualified Data.Record.Generic.Rep.Internal as Rep

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Anonymous record
--
-- To access fields of the record, either use the 'HasFie;d' instances
-- (possibly using the record-dot-preprocessor to get record-dot syntax),
-- or using the simple wrappers 'get' and 'set'. The 'HasField' instances
-- are resolved by the plugin, so be sure to use
--
-- > {-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}
newtype Record (r :: [(Symbol, Type)]) = MkR (Map String Any)

data Field l where
  Field :: KnownSymbol l => Proxy l -> Field l

instance (l ~ l', KnownSymbol l) => IsLabel l' (Field l) where
  fromLabel = Field (Proxy @l)

{-------------------------------------------------------------------------------
  User-visible API
-------------------------------------------------------------------------------}

empty :: Record '[]
empty = MkR Map.empty

insert :: Field l -> a -> Record r -> Record ('(l, a) ': r)
insert (Field l) a (MkR r) = MkR $ Map.insert (symbolVal l) (unsafeCoerce a) r

{-------------------------------------------------------------------------------
  Generics
-------------------------------------------------------------------------------}

class RecordConstraints (r :: [(Symbol, Type)]) (c :: Type -> Constraint) where
  dictRecord :: Proxy c -> Rep (Dict c) (Record r)

class RecordMetadata (r :: [(Symbol, Type)]) where
  recordMetadata :: Metadata (Record r)

instance RecordMetadata r => Generic (Record r) where
  type Constraints (Record r) = RecordConstraints r
  type MetadataOf  (Record r) = r

  dict       = dictRecord
  metadata _ = recordMetadata

  from :: Record r -> Rep I (Record r)
  from (MkR r) = Rep.unsafeFromListAny (aux $ Map.elems r)
    where
      aux :: [Any] -> [I Any]
      aux = coerce

  to :: Rep I (Record r) -> Record r
  to = error "to: not yet defined (we need to reconstruct the field names)"

{-------------------------------------------------------------------------------
  Instances

  These depend on generics.

  TODO: Should RecordMetadata be a superclass of RecordConstraints?
-------------------------------------------------------------------------------}

gshowRecord ::
     (RecordMetadata r, RecordConstraints r Show)
  => Record r -> String
gshowRecord = combine . Rep.collapse . Rep.cmap (Proxy @Show) aux . from
  where
    aux :: Show x => I x -> K String x
    aux (I x) = K (show x)

    combine :: [String] -> String
    combine fs = concat [
          "Record {"
        , intercalate ", " fs
        , "}"
        ]

{-------------------------------------------------------------------------------
  Internal API
-------------------------------------------------------------------------------}

-- | Used by the plugin during evidence construction for 'HasField'
--
-- Precondition: the record must have the specified field with type @a@
-- (this precondition is verified by the plugin before generating "evidence"
-- that uses this function).
unsafeRecordHasField :: forall r a. String -> Record r -> (a -> Record r, a)
unsafeRecordHasField label (MkR r) = (
      \a -> MkR $ Map.insert label (unsafeCoerce a) r
    , case Map.lookup label r of
        Just f  -> unsafeCoerce f
        Nothing -> error preconditionViolation
    )
  where
    preconditionViolation :: String
    preconditionViolation = concat [
          "unsafeRecordHasField precondition violation: field "
        , label
        , " not found"
        ]

-- | Used by the plugin during evidence construction for 'RecordConstraints'
unsafeDictRecord :: forall r c.
     [Dict c Any]  -- ^ Dictionary for each field, in order
  -> Proxy c
  -> Rep (Dict c) (Record r)
unsafeDictRecord ds _ = Rep.unsafeFromListAny ds

-- | Used by the plugin during evidence construction for 'RecordMetadata'
unsafeFieldMetadata :: forall r.
     [FieldMetadata Any]
  -> Rep FieldMetadata (Record r)
unsafeFieldMetadata = Rep.unsafeFromListAny