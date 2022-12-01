{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Control.Exception.Array
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
module Control.Exception.Array (
  -- * ElementError 
  ElementError (ElementErrorWithIndex, ElementError, ..),
  showElementErrorMessage,

  -- * IndexError
  IndexError (..),
  showIndexErrorMessage,
) where

import Control.Exception (Exception, displayException)

import Data.Data (Data)

import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift)

-- ElementError ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype ElementError = MkElementError 
  {getElementError :: Maybe Int}
  deriving (Data, Eq, Generic, Lift, Ord)

-- | TODO
--
-- @since 1.0.0
pattern ElementErrorWithIndex :: Int -> ElementError 
pattern ElementErrorWithIndex i = MkElementError (Just i)

-- | TODO
--
-- @since 1.0.0
pattern ElementError :: ElementError 
pattern ElementError = MkElementError Nothing

{-# COMPLETE ElementErrorWithIndex, ElementError #-}

-- | Renders the 'IndexError' via 'showIndexErrorMessage'.
--
-- @since 1.0.0
instance Exception ElementError where
  displayException = showElementErrorMessage
  {-# INLINE displayException #-}

-- | Renders the 'IndexError' via 'showIndexErrorMessage'.
--
-- @since 1.0.0
instance Show ElementError where 
  showsPrec _ exn = showString (showElementErrorMessage exn)
  {-# INLINE showsPrec #-}

-- | The implementation for 'displayException'. Renders the 'IndexError' as a
-- human-readable string.
showElementErrorMessage :: ElementError -> String
showElementErrorMessage (ElementErrorWithIndex i) = 
  "ElementError: evaluated undefined array element at index " ++ show i
showElementErrorMessage ElementError = 
  "ElementError: evaluated undefined array element" 

-- IndexError ------------------------------------------------------------------

-- | 'IndexError' represents an exception that is raised when an out-of-bounds 
-- index would be used to access an array.
--
-- @since 1.0.0
data IndexError = IndexError
  { arrayIndex :: Int
  -- ^ The offending index. 
  , arrayLength :: Int
  -- ^ The length of the array. 
  }
  deriving (Data, Eq, Generic, Lift, Ord)

-- | Renders the 'IndexError' via 'showIndexErrorMessage'.
--
-- @since 1.0.0
instance Exception IndexError where
  displayException = showIndexErrorMessage
  {-# INLINE displayException #-}

-- | Renders the 'IndexError' via 'showIndexErrorMessage'.
--
-- @since 1.0.0
instance Show IndexError where
  showsPrec _ exn = showString (showIndexErrorMessage exn)
  {-# INLINE showsPrec #-}

-- | The implementation for 'displayException'. Renders the 'IndexError' as a
-- human-readable string.
--
-- >>> showIndexErrorMessage (IndexError 5 3)
-- "IndexError: index out of bounds: length is 3 but the index is 5"
--
-- @since 1.0.0
showIndexErrorMessage :: IndexError -> String
showIndexErrorMessage exn =
  "IndexError: index out of bounds: length is "
    ++ shows (arrayLength exn) " but the index is "
    ++ show (arrayIndex exn)
