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
readDouble chars = maybe _error Right (readMaybe $ Char8.unpack chars)
  where
    _error = Left $ "expected double in place of '" ++ showText chars ++ "' "


