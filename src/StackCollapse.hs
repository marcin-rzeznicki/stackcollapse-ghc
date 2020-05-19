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
{-# LANGUAGE ExistentialQuantification #-}

module StackCollapse
    ( MayFail
    , StackCollapse(..)
    , collapseStack
    , stackCollapseFromPath
    , stackCollapseFromConfig) where

import           Trace
import           Config
import           UserModule
import           Orphans
import           Profiles
import           CallTree (CallForest)
import           Data.ByteString.Builder
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy
import           Data.Bool
import           Data.Text.Encoding (encodeUtf8Builder)
import           Data.List (intersperse)
import           Data.Functor.Foldable
import           Data.Functor ((<&>))
import           Control.Applicative (liftA2)

data StackCollapse = forall p. Profile p
  => StackCollapse { profile :: p
                   , builder :: Trace -> Builder
                   , mode :: OperationMode
                   }

collapseStack :: StackCollapse -> MayFail Lazy.ByteString
collapseStack StackCollapse { profile, builder, mode } =
  buildCallForest mode profile
  <&> collapseCallForest (collectSamples mode) builder
  where
    collectSamples Time = ticks
    collectSamples Alloc = alloc

stackCollapseFromConfig
  :: StackCollapseConfig -> ByteString.ByteString -> StackCollapse
stackCollapseFromConfig config input =
  let builder = frameBuilder config
      mode = operationMode config
  in case inputType config of
       Standard
         -> StackCollapse { profile = ProfileStandard input, builder, mode }
       Detailed
         -> StackCollapse { profile = ProfileDetailed input, builder, mode }

stackCollapseFromPath :: StackCollapseConfig -> FilePath -> IO StackCollapse
stackCollapseFromPath config = fmap (stackCollapseFromConfig config)
  . ByteString.readFile

collapseCallForest
  :: (Trace -> Integer) -> (Trace -> Builder) -> CallForest -> Lazy.ByteString
collapseCallForest collectSamples buildFrame = toLazyByteString
  . mconcat
  . intersperse (char7 '\n')
  . concatMap catamorphism
  where
    catamorphism = cata $ collapseCallTraces collectSamples buildFrame

collapseCallTraces :: (Trace -> Integer)
                   -> (Trace -> Builder)
                   -> TreeF Trace [Builder]
                   -> [Builder]
collapseCallTraces collectSamples buildFrame (NodeF trace stacks)
  | collectedSamples == 0 = stacks'
  | otherwise = frameWithCount:stacks'
  where
    collectedSamples = collectSamples trace

    frame = buildFrame trace

    stacks' = concatMap (map $ withSep ';' frame) stacks

    frameWithCount = withSep ' ' frame $ integerDec collectedSamples

frameBuilder :: StackCollapseConfig -> Trace -> Builder
frameBuilder config =
  let optSource =
        let builder = (char7 ' ' <>) . bracket . encodeUtf8Builder . source
        in case sourceMode config of
             SourceNever  -> const mempty
             SourceAlways -> builder <?> hasLocation
             SourceUser   -> builder
               <?> liftA2 (&&) hasLocation (isUserTrace config)
      optModule = let builder = (<> char7 '.') . encodeUtf8Builder . modul
                  in case functionNameMode config of
                       QualifiedNever  -> const mempty
                       QualifiedAlways -> builder <?> not . isMain
                       QualifiedKernel -> builder
                         <?> not . liftA2 (||) isMain (isUserTrace config)
      optAnnot = case annotationMode config of
        WithAnnotations -> annot
        NoAnnotations   -> const mempty
      annot trace
        | isCAF trace = annotate "i"
        | isKernelTrace trace = annotate "k"
        | isUserTrace config trace = annotate "j"
        | otherwise = mempty
  in optModule <> (encodeUtf8Builder . costCentre) <> optSource <> optAnnot

isUserTrace :: StackCollapseConfig -> Trace -> Bool
isUserTrace = inAnyUserModule . userModules

isKernelTrace :: Trace -> Bool
isKernelTrace = inAnyModule kernelModules
  where
    kernelModules =
      [ "GHC"
      , "Data.Tuple"
      , "Data.Bits"
      , "Control.Monad"
      , "Data.HashTable"
      , "Data.Typeable"
      , "Foreign"
      , "Numeric"
      , "System"
      , "Text"
      , "Type"
      , "Unsafe"]

withSep :: Char -> Builder -> Builder -> Builder
withSep sep left right = left <> char7 sep <> right

bracket :: Builder -> Builder
bracket b = char7 '[' <> b <> char7 ']'

annotate :: Builder -> Builder
annotate = (char7 '_' <>) . bracket

(<?>) :: (Monoid b) => (a -> b) -> (a -> Bool) -> a -> b
then' <?> if' = \a -> bool mempty (then' a) (if' a)

infixl 7 <?>
