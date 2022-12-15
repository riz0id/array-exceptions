{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Control.Exception.RangeError
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Control.Exception.RangeError (
  -- * RangeError
  RangeError (..),

  -- * RangePrefix
  RangePrefix (..),
) where

import Data.Data (Data)

import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift (..), Name, liftData, unsafeCodeCoerce)

-- RangeError ------------------------------------------------------------------

-- TODO: docs
--
-- @since 1.0.0
data RangeError = RangeError
  { -- | TODO: docs
    functionName :: Name
    -- | TODO: docs
  , typeName     :: Name
    -- | TODO: docs
  , indexPrefix  :: Maybe RangePrefix
    -- | TODO: docs
  , lowerBound   :: Int
    -- | TODO: docs
  , upperBound   :: Int
  }
  deriving (Data, Eq, Generic, Ord, Show)

-- | @since 1.0.0
instance Lift RangeError where
  lift = liftData
  {-# INLINE lift #-}

  liftTyped exn = unsafeCodeCoerce (lift exn)
  {-# INLINE liftTyped #-}

-- RangePrefix -----------------------------------------------------------------

-- TODO: docs
--
-- @since 1.0.0
data RangePrefix
  = -- | TODO: docs 
    PrefixStarting
    -- | TODO: docs
  | PrefixEnding
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Lift, Show)
