{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Record.Anonymous (
    Record       -- opaque
  , Field        -- opaque (use @#foo@ to create values)
  , empty
  , insert
  , get
  , set
    -- * Re-exports
  , HasField(..)
  ) where

import Data.Kind
import Data.Map (Map)
import Data.Proxy
import GHC.Exts (Any)
import GHC.OverloadedLabels
import GHC.Records.Compat
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Map as Map

-- | Anonymous record
--
-- To access fields of the record, either use the 'HasFie;d' instances
-- (possibly using the record-dot-preprocessor to get record-dot syntax),
-- or using the simple wrappers 'get' and 'set'. The 'HasField' instances
-- are resolved by the plugin, so be sure to use
--
-- > {-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}
newtype Record (r :: [(Symbol, Type)]) = MkR (Map String Any)

empty :: Record '[]
empty = MkR Map.empty

data Field l where
  Field :: KnownSymbol l => Proxy l -> Field l

instance (l ~ l', KnownSymbol l) => IsLabel l' (Field l) where
  fromLabel = Field (Proxy @l)

insert :: Field l -> a -> Record r -> Record ('(l, a) ': r)
insert (Field l) a (MkR r) = MkR $ Map.insert (symbolVal l) (unsafeCoerce a) r

-- | Get record field
--
-- This is a simple wrapper for 'getField'.
get :: forall l r a.
     HasField l (Record r) a
  => Field l -> Record r -> a
get _ = getField @l @(Record r)

-- | Set record field
--
-- This is a simple wrapper for 'setField'.
set :: forall l r a.
     HasField l (Record r) a
  => Field l -> a -> Record r -> Record r
set _ = flip (setField @l @(Record r))

