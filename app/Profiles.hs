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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Profiles (Standard(..), Detailed(..), Profile(..), MayFail) where

import           Format
import           CallTreeBuilder
import           Trace
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import           Data.Char (isSpace)
import           Control.Applicative (liftA2)
import           Control.Arrow (left)

newtype Detailed = ProfileDetailed ByteString

detailedProfStart :: Detailed -> [NumberedLine]
detailedProfStart (ProfileDetailed bs) = skipToMain $ numberedLines bs

newtype Standard = ProfileStandard ByteString

totalsStart :: Standard -> [NumberedLine]
totalsStart (ProfileStandard bs) = skipToTotalTime $ numberedLines bs

class Profile p where
  buildCallForest :: OperationMode -> p -> MayFail CallForest

instance Profile Detailed where
  buildCallForest opMode = tryBuildCallForest format opMode . detailedProfStart
    where
      format
        [_cc, _module, _src, _, _, _, _, _inhTime, _inhAlloc, _ticks, _bytes] =
        let readTrace = Trace <$> readCostCentre _cc
              <*> readText _module
              <*> readSrc _src
              <*> readInteger _ticks
              <*> readInteger _bytes
            readInherited =
              Inherited <$> readDouble _inhTime <*> readDouble _inhAlloc
        in liftA2 (,) readTrace readInherited
      format _ = Left
        "missing columns. Is the input file in the correct format (-P/-pa)?"

instance Profile Standard where
  buildCallForest opMode p = do
    totalTicks <- left ((printError "total ticks" $ fst totalTimeLine) ++)
      $ readTotalTicks
      $ snd totalTimeLine
    totalBytes <- left ((printError "total bytes" $ fst totalAllocLine) ++)
      $ readTotalBytes
      $ snd totalAllocLine
    tryBuildCallForest (format totalTicks totalBytes) opMode $ skipToMain start
    where
      start = totalsStart p

      totalTimeLine = head start

      totalAllocLine = head $ tail start

      format
        totalTicks
        totalBytes
        [_cc, _module, _src, _, _, _indTime, _indAlloc, _inhTime, _inhAlloc] =
        let readTrace =
              (\cc modul src indTime indAlloc
               -> let ticks = round $ indTime * fromIntegral totalTicks
                      alloc = round $ indAlloc * fromIntegral totalBytes
                  in Trace { cc, modul, src, ticks, alloc })
              <$> readCostCentre _cc
              <*> readText _module
              <*> readSrc _src
              <*> readDouble _indTime
              <*> readDouble _indAlloc
            readInherited =
              Inherited <$> readDouble _inhTime <*> readDouble _inhAlloc
        in liftA2 (,) readTrace readInherited
      format _ _ _ = Left
        "unexpected number of columns. Is the input file in the correct format (-p)?"

      printError what line = "confused at line "
        ++ show line
        ++ " when trying to parse "
        ++ what
        ++ ": "

skipToMain :: [NumberedLine] -> [NumberedLine]
skipToMain =
  skipEmpty . tail . skipToCostCentreHeader . tail . skipToCostCentreHeader
  where
    startsWithCostCentre = ("COST CENTRE" `Char8.isPrefixOf`) . snd

    skipToCostCentreHeader = dropWhile (not . startsWithCostCentre)

    skipEmpty = dropWhile (Char8.null . snd)

skipToTotalTime :: [NumberedLine] -> [NumberedLine]
skipToTotalTime = dropWhile (not . startsWithTotalTime)
  where
    startsWithTotalTime = ("total time" `Char8.isPrefixOf`) . trimLeft . snd

readTotalTicks :: ByteString -> MayFail Integer
readTotalTicks = readInteger . Char8.tail . trim . atOpeningParen
  where
    atOpeningParen = Char8.dropWhile (/= '(')

readTotalBytes :: ByteString -> MayFail Integer
readTotalBytes = readGroupedDecimal . trim . Char8.drop 2 . atEqualSign
  where
    atEqualSign = Char8.dropWhile (/= '=')

    readGroupedDecimal = go 0
      where
        go r bs = case Char8.readInteger bs of
          Just (i, bs')
            -> let j = r * 1000 + i
               in case Char8.uncons bs' of
                    Just (',', bs'') -> go j bs''
                    Just (ch, _)
                      -> Left $ "unexpected '" ++ ch:"' when looking for ','"
                    Nothing          -> Right j
          Nothing
            -> Left $ "expected integer but found '" ++ Char8.unpack bs ++ "' "

trimLeft :: ByteString -> ByteString
trimLeft = Char8.dropWhile isSpace

trim :: ByteString -> ByteString
trim = Char8.takeWhile (not . isSpace)



