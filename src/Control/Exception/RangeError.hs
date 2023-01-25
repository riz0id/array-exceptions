{-# LANGUAGE PatternSynonyms #-}
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
module Control.Exception.RangeError
  ( -- * Range Errors
    RangeError (StartingRangeError, EndingRangeError, ..)
    -- ** Query
  , isEmptyRangeError
    -- * Range Prefixes
  , RangePrefix (..)
    -- ** Show
  , showRangePrefix
  , describeRangePrefix
  ) where

import Control.Exception (Exception, displayException)

import Data.Data (Data)

import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift (..), Name)
import Language.Haskell.TH.Syntax qualified as TH

-- Range Errors ----------------------------------------------------------------

-- | The 'RangeError' type represents an exception that reports out-of-bounds 
-- errors in functions used to read, write, or slice container-like 
-- data structures such as lists, arrays, buffers, or vectors.   
--
-- @since 1.0.0
data RangeError = RangeError
  { -- | The 'function' field stores 'Name' of the function the that the
    -- 'RangeError' was thrown from. 
    function   :: Name
    -- | The 'type_name' field refers to the name of the type that the 
    -- 'position' is indexing. This is typically the double-quoted name of 
    -- containers such as a list, map, or buffer.
  , type_name   :: Name
    -- | The 'prefix' is an optional field that alters the error message 
    -- produced when a 'RangeError' is rendered via 'show' or 'displayException' 
    -- functions. When a 'prefix' is specified, a "starting" or "ending" prefix 
    -- will be prepended to any mentions of the 'position' field via the 
    -- 'describeRangePrefix' function, for example @('Just' 'PrefixStarting')@:
    --
    -- >>> describeRangePrefix PrefixStarting 10
    -- "starting position: 10"
    --
    -- Otherwise, the rendered error message will refer to the 'position' as 
    -- "position". 
  , prefix     :: Maybe RangePrefix
    -- | The 'position' field stores the /invalid/ position provided as an 
    -- argument to function named in the 'function' field that caused the 
    -- 'RangeError' to be thrown.
  , position   :: {-# UNPACK #-} !Int
    -- | The 'lower_bound' field is smallest valid position that the 'function' 
    -- would accept as an argument.
  , lower_bound :: {-# UNPACK #-} !Int
    -- | The 'upper_bound' field is largest valid position that the 'function' 
    -- would accept as an argument. Note that if the value for 'upper_bound' is 
    -- less than the value for 'lower_bound', then the 'RangeError' is 
    -- considered to have an empty range. See 'isEmptyRangeError'.
  , upper_bound :: {-# UNPACK #-} !Int
  }
  deriving (Data, Eq, Generic, Ord)

-- | @since 1.0.0
instance Exception RangeError where 
  displayException = show 
  {-# INLINE displayException #-}

-- | @since 1.0.0
instance Lift RangeError where
  lift = TH.liftData
  {-# INLINE lift #-}

  liftTyped exn = TH.unsafeCodeCoerce (lift exn)
  {-# INLINE liftTyped #-}

-- | @since 1.0.0
instance Show RangeError where
  showsPrec _ exn
    | isEmptyRangeError exn =
      descName
        . showString ": "
        . descPosition
        . showString " is out of range for empty "
        . descType
        . showString "\n  "
        . notePosition
    | otherwise =
      descName
        . showString ": "
        . descPosition
        . showString " is out of range for "
        . descType
        . showString "\n  "
        . notePosition
        . showString "\n  valid range: ["
        . shows (lower_bound exn)
        . showString ", "
        . shows (upper_bound exn)
        . showString "]"
    where
      descName :: ShowS
      descName = showString (TH.nameBase (function exn))

      descType :: ShowS
      descType = showString (TH.nameBase (type_name exn))

      descPosition :: ShowS
      descPosition = maybe (showString "position") showRangePrefix (prefix exn)

      notePosition :: ShowS
      notePosition = case prefix exn of
        Nothing -> showString "position: " . shows (position exn)
        Just x  -> describeRangePrefix x (position exn)
  {-# INLINE showsPrec #-}

-- Range Errors - Construction -------------------------------------------------

-- | 'StartingRangeError' is a pattern synonym for constructing a 'RangeError' 
-- using @('Just' 'PrefixStarting')@ as the value for 'prefix' field.
--
-- @since 1.0.0
pattern StartingRangeError :: Name -> Name -> Int -> Int -> Int -> RangeError
pattern StartingRangeError fun ty pos lower upper = 
  RangeError fun ty (Just PrefixStarting) pos lower upper

-- | 'EndingRangeError' is a pattern synonym for constructing a 'RangeError' 
-- using @('Just' 'PrefixEnding')@ as the value for 'prefix' field.
--
-- @since 1.0.0
pattern EndingRangeError :: Name -> Name -> Int -> Int -> Int -> RangeError
pattern EndingRangeError fun ty pos lower upper = 
  RangeError fun ty (Just PrefixEnding) pos lower upper

{-# COMPLETE StartingRangeError, EndingRangeError #-}

-- Range Errors - Query --------------------------------------------------------

-- | Is the range for the given 'RangeError' empty? 
--
-- @since 1.0.0
isEmptyRangeError :: RangeError -> Bool
isEmptyRangeError exn = upper_bound exn < lower_bound exn
{-# INLINE isEmptyRangeError #-}

-- Range Prefixes --------------------------------------------------------------

-- | 'RangePrefix' is an optional descriptor that can to disambiguate which 
-- 'position' caused a 'RangeError' to be thrown in situations where two  
-- positions are being used, such as in the case of array slicing functions that 
-- take a starting index and ending index.
--
-- @since 1.0.0
data RangePrefix
  -- | 'PrefixStarting' is used to prefix the 'position' field of a 'RangeError'
  -- with "starting": 
  --
  -- >>> showRangePrefix PrefixStarting 
  -- "starting position"
  = PrefixStarting
  -- | 'PrefixEnding' is used to prefix the 'position' field of a 'RangeError'
  -- with "ending": 
  --
  -- >>> showRangePrefix PrefixEnding 
  -- "ending position"
  | PrefixEnding
  deriving (Bounded, Data, Enum, Eq, Generic, Lift, Ord, Show)

-- Range Prefixes - Show -------------------------------------------------------

-- | Renders a 'RangePrefix' in pretty-printed format. 
--
-- >>> (showRangePrefix PrefixStarting, showRangePrefix PrefixEnding)
-- ("starting position", "ending position")
--
-- @since 1.0.0
showRangePrefix :: RangePrefix -> ShowS
showRangePrefix PrefixStarting = showString "starting position"
showRangePrefix PrefixEnding   = showString "ending position"
{-# INLINE showRangePrefix #-}

-- | Renders a value of type @a@ along with a pretty-printed 'RangePrefix' 
-- prepended. 'describeRangePrefix' is used to display note the 'position' in
-- the 'show' and 'displayException' implementations for 'RangeError'.
--
-- >>> (describeRangePrefix PrefixStarting 10, describeRangePrefix PrefixEnding 10)
-- ("starting position: 10", "ending position: 10")
--
-- @since 1.0.0
describeRangePrefix :: Show a => RangePrefix -> a -> ShowS
describeRangePrefix x i = showRangePrefix x . showString ": " . shows i

{-# SPECIALISE INLINE describeRangePrefix :: RangePrefix -> Int -> ShowS #-}