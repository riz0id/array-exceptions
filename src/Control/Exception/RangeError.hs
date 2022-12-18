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

  -- ** Query
  isEmptyRangeError,

  -- * RangePrefix
  RangePrefix (..),

  -- ** Show
  showRangePrefix,
  describeRangePrefix,
) where

import Data.Data (Data)

import GHC.Generics (Generic)

import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (..), Name, liftData, unsafeCodeCoerce)
import Control.Exception (Exception)

-- RangeError ------------------------------------------------------------------

-- TODO: docs
--
-- @since 1.0.0
data RangeError = RangeError
  { -- | TODO: docs
    name :: Name
    -- | TODO: docs
  , typeName     :: Name
    -- | TODO: docs
  , prefix  :: Maybe RangePrefix
    -- | TODO: docs
  , index :: Int
    -- | TODO: docs
  , lowerBound   :: Int
    -- | TODO: docs
  , upperBound   :: Int
  }
  deriving (Data, Eq, Generic, Ord)

-- | @since 1.0.0
instance Exception RangeError

-- | @since 1.0.0
instance Lift RangeError where
  lift = liftData
  {-# INLINE lift #-}

  liftTyped exn = unsafeCodeCoerce (lift exn)
  {-# INLINE liftTyped #-}

-- | @since 1.0.0
instance Show RangeError where
  showsPrec _ exn
    | isEmptyRangeError exn =
      descName
        . showString ": "
        . descIndex 
        . showString " is out of range for empty "
        . descType
        . showString "\n  "
        . noteIndex 
    | otherwise =
      descName
        . showString ": "
        . descIndex 
        . showString " is out of range for "
        . descType
        . showString "\n  "
        . noteIndex 
        . showString "\n  valid range: ["
        . shows (lowerBound exn)
        . showString ", "
        . shows (upperBound exn)
        . showString "]"
    where 
      descName :: ShowS 
      descName = showString (TH.nameBase (name exn))

      descType :: ShowS 
      descType = showString (TH.nameBase (typeName exn))

      descIndex :: ShowS
      descIndex = maybe (showString "index") showRangePrefix (prefix exn)

      noteIndex :: ShowS 
      noteIndex = case prefix exn of 
        Nothing -> showString "index: " . shows (index exn)
        Just x  -> describeRangePrefix x (index exn)
  {-# INLINE showsPrec #-}

-- RangeError - Query ----------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
isEmptyRangeError :: RangeError -> Bool
isEmptyRangeError exn = upperBound exn < lowerBound exn

-- RangePrefix -----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data RangePrefix
  -- | TODO: docs
  = PrefixStarting
  -- | TODO: docs
  | PrefixEnding
  deriving (Bounded, Data, Enum, Eq, Generic, Lift, Ord, Show)

-- RangePrefix - Show ----------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
showRangePrefix :: RangePrefix -> ShowS
showRangePrefix PrefixStarting = showString "starting index"
showRangePrefix PrefixEnding   = showString "ending index"

-- | TODO: docs
--
-- @since 1.0.0
describeRangePrefix :: RangePrefix -> Int -> ShowS
describeRangePrefix x i = showRangePrefix x . showString ": " . shows i
