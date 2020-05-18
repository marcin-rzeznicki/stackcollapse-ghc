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

module Trace
    ( Trace(..)
    , CostCentre(..)
    , Src(..)
    , hasLocation
    , isMain
    , isCAF
    , costCentre
    , source
    , inAnyModule) where

import           Data.Text (Text)
import qualified Data.Text as T

data Trace = Trace { cc :: CostCentre
                   , modul :: Text
                   , src :: Src
                   , ticks :: Integer
                   , alloc :: Integer
                   }

data CostCentre = Main
                | CAF (Maybe Text)
                | SCC Text

data Src = BuiltIn
         | EntireModule
         | NoLocationInfo
         | Location Text

hasLocation :: Trace -> Bool
hasLocation Trace { src = (Location _) } = True
hasLocation _ = False

isMain :: Trace -> Bool
isMain Trace { cc = Main } = True
isMain _ = False

isCAF :: Trace -> Bool
isCAF Trace { cc = (CAF _) } = True
isCAF _ = False

costCentre :: Trace -> Text
costCentre trace = case cc trace of
  Main -> "MAIN"
  CAF Nothing -> "CAF"
  CAF (Just text) -> text
  SCC text -> text

source :: Trace -> Text
source trace = case src trace of
  BuiltIn        -> "<built-in>"
  EntireModule   -> "<entire-module>"
  NoLocationInfo -> "<no location info>"
  Location l     -> l

inAnyModule :: [Text] -> Trace -> Bool
inAnyModule modules Trace { modul } = any
  (maybe False (\suffix -> T.null suffix || T.head suffix == '.')
   . flip T.stripPrefix modul)
  modules
