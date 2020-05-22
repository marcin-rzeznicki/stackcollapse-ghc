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
import           Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NEL
import           Data.Either.Extra (maybeToEither)
import           Control.Applicative (liftA2)
import           Control.Arrow (left)
import           Control.Monad
import           Safe (tailMay)

newtype Detailed = ProfileDetailed ByteString

detailedProfStart :: Detailed -> MayFail [NumberedLine]
detailedProfStart (ProfileDetailed bs) = skipToMain $ numberedLines bs

newtype Standard = ProfileStandard ByteString

totalsStart :: Standard -> MayFail (NonEmpty NumberedLine)
totalsStart (ProfileStandard bs) = skipToTotalTime $ numberedLines bs

class Profile p where
  buildCallForest :: OperationMode -> p -> MayFail CallForest

instance Profile Detailed where
  buildCallForest
    opMode = detailedProfStart >=> tryBuildCallForest format opMode
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
    start <- totalsStart p
    let (totalTimeLine, afterTotalTime) = NEL.uncons start
    totalTicks <- left ((printError "total ticks" $ fst totalTimeLine) ++)
      $ readTotalTicks
      $ snd totalTimeLine
    (totalAllocLine :| afterTotals)
      <- maybeToEither (eofError "after the total ticks line") afterTotalTime
    totalBytes <- left ((printError "total bytes" $ fst totalAllocLine) ++)
      $ readTotalBytes
      $ snd totalAllocLine
    input <- skipToMain afterTotals
    tryBuildCallForest (format totalTicks totalBytes) opMode input
    where
      format
        totalTicks
        totalBytes
        [_cc, _module, _src, _, _, _indTime, _indAlloc, _inhTime, _inhAlloc] =
        let readTrace =
              (\cc modul src indTime indAllc
               -> let ticks = round $ indTime * (fromIntegral totalTicks / 100)
                      alloc = round $ indAllc * (fromIntegral totalBytes / 100)
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
        "unexpected number of columns. Is the input file in the correct format (-p)? Did you use the '-p' option?"

      printError what line = "confused at line "
        ++ show line
        ++ " when trying to parse '"
        ++ what
        ++ "': "

skipToMain :: [NumberedLine] -> MayFail [NumberedLine]
skipToMain = fmap skipEmpty
  . maybeToEither (eofError "can't find the header")
  . (tailMay . skipToCostCentreHeader >=> tailMay . skipToCostCentreHeader)
  where
    startsWithCostCentre = ("COST CENTRE" `Char8.isPrefixOf`) . snd

    skipToCostCentreHeader = dropWhile (not . startsWithCostCentre)

    skipEmpty = dropWhile (Char8.null . snd)

skipToTotalTime :: [NumberedLine] -> MayFail (NonEmpty NumberedLine)
skipToTotalTime = maybeToEither (eofError "can't find the total time line")
  . nonEmpty
  . dropWhile (not . startsWithTotalTime)
  where
    startsWithTotalTime = ("total time" `Char8.isPrefixOf`) . trimLeft . snd

eofError :: String -> String
eofError why = "unexpected end of file: "
  ++ why
  ++ " \n(This usually means that the file is malformed. If you're sure that the file is correct then you found a bug)"

readTotalTicks :: ByteString -> MayFail Integer
readTotalTicks = readInteger . Char8.drop 1 . trim . atOpeningParen
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



