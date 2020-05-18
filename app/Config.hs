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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Config
    ( StackCollapseConfig(..)
    , defaultConfig
    , InputType(..)
    , SourceMode(..)
    , FunctionNameMode(..)
    , OperationMode(..)
    , AnnotationMode(..)
    , prependUserModule) where

import           UserModule

data StackCollapseConfig =
  Config { userModules :: [UserModule]
         , sourceMode :: SourceMode
         , functionNameMode :: FunctionNameMode
         , operationMode :: OperationMode
         , annotationMode :: AnnotationMode
         , inputType :: InputType
         }

defaultConfig :: StackCollapseConfig
defaultConfig = Config { userModules = []
                       , sourceMode = SourceNever
                       , functionNameMode = QualifiedKernel
                       , operationMode = Time
                       , annotationMode = WithAnnotations
                       , inputType = Detailed
                       }

data InputType = Standard
               | Detailed

data SourceMode = SourceAlways
                | SourceUser
                | SourceNever
  deriving stock (Eq)

data FunctionNameMode = QualifiedAlways
                      | QualifiedKernel
                      | QualifiedNever
  deriving stock (Eq)

data OperationMode = Time
                   | Alloc

data AnnotationMode = NoAnnotations
                    | WithAnnotations
  deriving stock (Eq)

prependUserModule :: StackCollapseConfig -> UserModule -> StackCollapseConfig
prependUserModule config @ Config { userModules } modu =
  config { userModules = modu:userModules }



