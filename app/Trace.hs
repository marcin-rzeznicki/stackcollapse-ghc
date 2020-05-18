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
