{-# LANGUAGE TemplateHaskellQuotes #-}

module Main where

import Control.Exception.RangeError

import Data.Primitive.SmallArray

import GHC.Exts (fromList)

--------------------------------------------------------------------------------

lookupSmallArray :: SmallArray a -> Int -> Either RangeError a
lookupSmallArray src i
  | i < 0     =
    -- If the index @i@ is less than the lower bound @0@, return a 'RangeError'
    -- reporting that
    Left (RangeError 'lookupSmallArray ''SmallArray Nothing i 0 len)
  | i >= len  =
    Left (RangeError 'lookupSmallArray ''SmallArray Nothing i 0 len)
  | otherwise =
    Right (indexSmallArray src i)
  where
    len :: Int
    len = sizeofSmallArray src

main :: IO ()
main = do
  let array = fromList [1 .. 5] :: SmallArray Int

  case lookupSmallArray array 5 of 
    Left exn -> print exn
    Right x  -> print x
