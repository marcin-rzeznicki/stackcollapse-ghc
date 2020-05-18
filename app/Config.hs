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
prependUserModule config@Config { userModules } modu =
  config { userModules = modu:userModules }



