{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Data.Record.Anonymous (
    Record       -- opaque
  , Field        -- opaque (use @#foo@ to create values)
    -- * Core API
  , empty
  , insert
    -- * Additional convenience functions
  , get
  , set
    -- ** Re-exports
  , HasField(..)
  ) where

import GHC.Records.Compat

import Data.Record.Anonymous.Internal

{-------------------------------------------------------------------------------
  Convenience functions
-------------------------------------------------------------------------------}

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
