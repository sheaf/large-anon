{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Record.Anonymous.Internal (
    -- * Types
    Record(..)
  , Field(..)
    -- * User-visible API
  , empty
  , insert
    -- * Internal API
  , unsafeRecordHasField
  ) where

import Data.Kind
import Data.Map (Map)
import Data.Proxy
import GHC.Exts (Any)
import GHC.OverloadedLabels
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Map as Map

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
  Internal API
-------------------------------------------------------------------------------}

-- | Suitable implementation for a (plugin derived) 'HasField' instance
--
-- Precondition: the record must have the specified field with type @a@
-- (this precondition is verified by the plugin before generating "evidence"
-- that uses this function)
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




