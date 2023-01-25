{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Control.Exception.ElementError
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- An exception type for undefined element access on container-like structures 
-- such as arrays, maps, or vectors.
--
-- The 'Control.Exception.UndefinedElement' exception that is already provided 
-- by "base" exists for this purpose, but accepts any 'String' as the error 
-- message. The 'ElementError' improves on 'Control.Exception.UndefinedElement'
-- by:
--
--   * Standardizing information that is reported by undefined element 
--     exceptions. 
-- 
--   * Standardizing the error messages that are rendered by 'displayException'
--     or 'show', rather than accepting any 'String' as the message reported 
--     when the exception is arised.
--
-- == Example 1
--
-- Temporary undefined elements are sometimes necessary when using arrays from 
-- GHC's "primitive" library. For example, when implementing a function for 
-- constructing a new 'Data.Primitive.MutableArray' that is empty:
--
-- @
-- emptyMutArray :: PrimMonad m => m (MutableArray (PrimState s) a)
-- emptyMutArray = newArray 0 (throw ('ElementErrorAtLoc' 'MutableArray 0))
-- @
--
-- In this example, @(throw ('ElementErrorAtLoc' 'MutableArray 0))@ can be given 
-- to the @newArray@ function as the default element, which is required. This
-- is safe since the array is empty, and the 'Control.Exception.throw' thunk can 
-- never be read from the array and evaluated. 
--
-- == Example 2
--
-- Another situation where 
-- emphemeral undefined elements are necessary is when writing functions that 
-- generate the initial array elements with an index, for example:
--
-- @ 
-- generate :: Int -> (Int -> a) -> Array a
-- generate len f = runST do 
--   mut <- newArray len (throw ('ElementErrorNoLoc' 'MutableArray))
--
--   for_ [0 .. len - 1] \i -> 
--     writeArray mut i (f i) 
-- 
--   unsafeFreezeMutableArray mut
-- @ 
--
-- In this example, we must construct a 'MutableArray' to write elements to 
-- before we can begin iterating over each array index to generate the initial 
-- elements with the function argument @f@. By initialing all elements of the 
-- array to be @(throw ('ElementErrorNoLoc' 'MutableArray))@ thunks, we can 
-- safely construct the array ahead of time. Since the array elements are 
-- immediately replaced with the results of the function @f@, it is impossible 
-- to read and evaluate the original thunks which would throw an 
-- 'ElementErrorNoLoc' exception.
--
-- @since 1.0.0
module Control.Exception.ElementError
  ( -- * Range Errors
    ElementError (.., ElementErrorNoLoc, ElementErrorAtLoc)
  ) where

import Control.Exception (Exception, displayException)

import Data.Data (Data)

import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift (..), Name)
import Language.Haskell.TH.Syntax qualified as TH

-- Element Errors --------------------------------------------------------------

-- | The 'ElementError' type represents an exception that can be used in place 
-- of undefined elements or uninitialized elements within containers.
--
-- @since 1.0.0
data ElementError = ElementError
  { -- |  The 'type_name' field refers to the name of the type that the
    -- 'position' is indexing. This is typically the double-quoted name of
    -- containers such as a list, map, or buffer.
    type_name :: Name
    -- | The optional 'position' field stores the position of the undefined
    -- element within the array.
  , position  :: Maybe Int
  }
  deriving (Data, Eq, Generic, Ord)

-- | @since 1.0.0
instance Exception ElementError where
  displayException = show
  {-# INLINE displayException #-}

-- | @since 1.0.0
instance Lift ElementError where
  lift = TH.liftData
  {-# INLINE lift #-}

  liftTyped exn = TH.unsafeCodeCoerce (lift exn)
  {-# INLINE liftTyped #-}

-- | @since 1.0.0
instance Show ElementError where
  showsPrec _ exn =
    showString "element error: uninitialized "
      . showString (TH.nameBase (type_name exn))
      . showString "array element"
      . notePosition
    where
      notePosition :: ShowS
      notePosition = case position exn of
        Nothing -> id
        Just i  -> showString "\n  position: " . shows i
  {-# INLINE showsPrec #-}

-- Element Errors - Construction -----------------------------------------------

-- | 'ElementErrorNoLoc' is a pattern synonym for constructing an 'ElementError'
-- without a position of the undefined element.
--
-- @since 1.0.0
pattern ElementErrorNoLoc :: Name -> ElementError
pattern ElementErrorNoLoc name = ElementError name Nothing

-- | 'ElementErrorAtLoc' is a pattern synonym for constructing an 'ElementError'
-- with a the position of the undefined element.
--
-- @since 1.0.0
pattern ElementErrorAtLoc :: Name -> Int -> ElementError
pattern ElementErrorAtLoc name i = ElementError name (Just i)

{-# COMPLETE ElementErrorNoLoc, ElementErrorAtLoc #-}

