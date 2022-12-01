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
  -- * Allocation Exceptions
  AllocError (..),
  showAllocErrorMessage,

  -- * Undefined Element Exceptions
  ElementError (ElementErrorWithIndex, ElementError, ..),
  showElementErrorMessage,

  -- * Out of Bounds Indexing Exceptions
  IndexError (..),
  showIndexErrorMessage,
) where

import Control.Exception (Exception, displayException)

import Data.Data (Data)

import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift)

-- Allocation Exceptions -------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data AllocError
  = -- | TODO: docs
    AllocError Int
  | -- | TODO: docs
    AlignError Int
  deriving (Data, Eq, Generic, Lift, Ord)

-- | Renders the 'AllocError' via 'showAllocErrorMessage'.
--
-- @since 1.0.0
instance Exception AllocError where
  displayException = showAllocErrorMessage
  {-# INLINE displayException #-}

-- | Renders the 'AllocError' via 'showAllocErrorMessage'.
--
-- @since 1.0.0
instance Show AllocError where
  showsPrec _ exn = showString (showAllocErrorMessage exn)
  {-# INLINE showsPrec #-}

-- | The implementation for 'displayException'. Renders the 'AllocError' as a
-- human-readable string. 
--
-- For v'AllocError' exceptions, the rendered error message is:
-- 
-- >>> showAllocErrorMessage (AllocError (- 5))
-- "AllocError: failed to allocate array of length -5"
--
-- For v'AlignError' exceptions, the rendered error message is:
--
-- >>> showAllocErrorMessage (AlignError 0)
-- "AlignError: cannot allocate array aligned to 0 bytes"
--
-- @since 1.0.0
showAllocErrorMessage :: AllocError -> String
showAllocErrorMessage (AllocError n) = 
  "AllocError: failed to allocate array of length " ++ show n
showAllocErrorMessage (AlignError n) = 
  "AlignError: cannot allocate array aligned to " ++ shows n " bytes"

-- Undefined Element Exceptions ------------------------------------------------

-- | 'ElementError' represents an exception that is used to report when an 
-- undefined array element is evaluated.  
--
-- @since 1.0.0
newtype ElementError = MkElementError
  {getElementError :: Maybe Int}
  deriving (Data, Eq, Generic, Lift, Ord)

-- | The 'ElementErrorWithIndex' pattern constructs an 'ElementError' with the  
-- index of the array element that triggered the exception.
--
-- @since 1.0.0
pattern ElementErrorWithIndex :: Int -> ElementError
pattern ElementErrorWithIndex i = MkElementError (Just i)

-- | The 'ElementError' pattern constructs an 'ElementError' for an unknown or 
-- unspecified array index.
--
-- @since 1.0.0
pattern ElementError :: ElementError
pattern ElementError = MkElementError Nothing

{-# COMPLETE ElementErrorWithIndex, ElementError #-}

-- | Renders the 'ElementError' via 'showElementErrorMessage'.
--
-- @since 1.0.0
instance Exception ElementError where
  displayException = showElementErrorMessage
  {-# INLINE displayException #-}

-- | Renders the 'ElementError' via 'showElementErrorMessage'.
--
-- @since 1.0.0
instance Show ElementError where
  showsPrec _ exn = showString (showElementErrorMessage exn)
  {-# INLINE showsPrec #-}

-- | The implementation for 'displayException'. Renders the 'ElementError' as a
-- human-readable string. 
--
-- If the 'ElementError' is given the array index of the element that triggered
-- the exception (i.e. the 'ElementError' was constructed via an
-- 'ElementErrorWithIndex' pattern), then the array index is noted in rendered 
-- error message: 
--
-- >>> ElementErrorWithIndex 5
-- ElementError: evaluated undefined array element at index 5
--
-- If the index of the array element is unknown or unspecified (i.e. the 
-- 'ElementError' was constructed via an v'ElementError' pattern), then the 
-- rendered error message is:
--
-- >>> ElementError
-- ElementError: evaluated undefined array element
--
-- @since 1.0.0
showElementErrorMessage :: ElementError -> String
showElementErrorMessage (ElementErrorWithIndex i) =
  "ElementError: evaluated undefined array element at index " ++ show i
showElementErrorMessage ElementError =
  "ElementError: evaluated undefined array element"

-- Out of Bounds Indexing Exceptions -------------------------------------------

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
