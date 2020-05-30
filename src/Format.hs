{-  stackcollapse-ghc - fold GHC prof files into flamegraph input
    Copyright (C) 2020 Marcin Rzeźnicki

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}

module Format
    ( MayFail
    , ColumnList
    , Format
    , Inherited(..)
    , readSrc
    , readCostCentre
    , readText
    , readInteger
    , readDouble) where

import           Trace
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8')
import           Data.Either.Extra (maybeToEither)
import           Control.Arrow (left)
import           Text.Read (readMaybe)

type MayFail = Either String

type ColumnList = [ByteString]

type Format = ColumnList -> MayFail (Trace, Inherited)

data Inherited =
  Inherited { inheritedTime :: Double, inheritedAlloc :: Double }

readSrc :: ByteString -> MayFail Src
readSrc = fmap mkSrc . readText
  where
    mkSrc "<built-in>" = BuiltIn
    mkSrc "<entire-module>" = EntireModule
    mkSrc "<no location info>" = NoLocationInfo
    mkSrc text = Location text

readCostCentre :: ByteString -> MayFail CostCentre
readCostCentre = fmap mkCC . readText
  where
    mkCC "MAIN" = Main
    mkCC "CAF" = CAF Nothing
    mkCC text
      | "CAF:" `T.isPrefixOf` text = CAF (Just text)
      | otherwise = SCC text

readText :: ByteString -> MayFail Text
readText = left show . decodeUtf8'

showText :: ByteString -> String
showText = either (const "<malformed UTF-8>") T.unpack . readText

readInteger :: ByteString -> MayFail Integer
readInteger chars = case Char8.readInteger chars of
  Just (i, chars')
    | Char8.null chars' -> Right i
    | otherwise -> _error
  Nothing          -> _error
  where
    _error = Left $ "expected integer in place of '" ++ showText chars ++ "' "

readDouble :: ByteString -> MayFail Double
readDouble chars = maybeToEither _error $ readMaybe $ Char8.unpack chars
  where
    _error = "expected double in place of '" ++ showText chars ++ "' "
