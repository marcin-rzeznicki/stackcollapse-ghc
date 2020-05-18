{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module CallTreeBuilder
    ( NumberedLine
    , numberedLines
    , tryBuildCallForest
    , CallForest
    , OperationMode) where

import           Format (MayFail, ColumnList, Format, Inherited(..))
import           CallTree
import           Config (OperationMode(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import           Control.Foldl (FoldM(..), foldM)
import           Control.Monad.Trans.State
import           Control.Arrow

type NumberedLine = (Int, ByteString)

numberedLines :: ByteString -> [NumberedLine]
numberedLines = zip [1 :: Int ..] . Char8.lines

tryBuildCallForest
  :: Format -> OperationMode -> [NumberedLine] -> MayFail CallForest
tryBuildCallForest format opMode nls =
  evalStateT (foldM builder $ map (second splitLine) nls) Building
  where
    builder = callTreeBuilder format opMode

type CallTreeBuilder = FoldM (StateT BuilderState MayFail) NumberedRow CallForest

type NumberedRow = (Int, Row)

data BuilderState = Skipping Int
                  | Building

callTreeBuilder :: Format -> OperationMode -> CallTreeBuilder
callTreeBuilder format opMode = FoldM
  (\tree -> StateT . build tree)
  (return EmptyTree)
  (return . extractForest)
  where
    build callTree (_, Row { level }) (Skipping depth)
      | level > depth = return (callTree, Skipping depth)
    build callTree (lineNumber, Row { level, columns }) _ = do
      (trace, inherited) <- format' lineNumber columns
      a <- insertTrace trace level callTree
      let s = if skip inherited
              then Skipping level
              else Building
      return (a, s)

    skip = case opMode of
      Time  -> (== 0.0) . inheritedTime
      Alloc -> (== 0.0) . inheritedAlloc

    format' lineNumber = left (printErrorLocation lineNumber ++) . format

    printErrorLocation
      lineNumber = "confused at line " ++ show lineNumber ++ ": "

data Row = Row { level :: !Int, columns :: ColumnList }

splitLine :: ByteString -> Row
splitLine line = Row { level = Char8.length ident, columns = splitLine' rest }
  where
    (ident, rest) = Char8.span (== ' ') line

    splitLine' bs
      | Char8.null bs = []
      | otherwise = case Char8.head bs of
        ' ' -> splitLine' $ Char8.dropWhile (== ' ') bs
        '<'
          | "<no location info>" `Char8.isPrefixOf` bs
            -> let (nli, bs') = Char8.splitAt 18 bs
               in nli:splitLine' bs'
        _   -> let (column, bs') = Char8.break (== ' ') bs
               in column:splitLine' bs'


