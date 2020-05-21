{-# LANGUAGE NamedFieldPuns #-}

module Helpers (embedInFileName, toArgsString, userModuleFromString) where

import           Config
import           UserModule (UserModule, userModule_Main)
import qualified UserModule
import           Data.Either.Extra (fromRight')

embedInFileName :: StackCollapseConfig -> String
embedInFileName
  Config { operationMode
         , sourceMode
         , functionNameMode
         , annotationMode
         , userModules
         , inputType
         } = embedInputType inputType
  ++ embedOperationMode operationMode
  ++ embedSourceMode sourceMode
  ++ embedFunctionNameMode functionNameMode
  ++ embedAnnotationMode annotationMode
  ++ embedUserModules userModules
  where
    embedInputType Standard = "-p"
    embedInputType Detailed = ""

    embedUserModules = concatMap (('+':) . UserModule.toString)
      . filter (/= userModule_Main)

    embedOperationMode Time = "--time"
    embedOperationMode Alloc = "--alloc"

    embedSourceMode SourceAlways = "--source-always"
    embedSourceMode SourceUser = "--source-user"
    embedSourceMode SourceNever = ""

    embedFunctionNameMode QualifiedAlways = "--qualified"
    embedFunctionNameMode QualifiedKernel = ""
    embedFunctionNameMode QualifiedNever = "--no-qualified"

    embedAnnotationMode WithAnnotations = ""
    embedAnnotationMode NoAnnotations = "--no-annot"

toArgsString :: StackCollapseConfig -> String
toArgsString
  Config { operationMode
         , sourceMode
         , functionNameMode
         , annotationMode
         , userModules
         , inputType
         } = unwords
  $ [ toArgs_Op operationMode
    , toArgs_Source sourceMode
    , toArgs_FunctionName functionNameMode
    , toArgs_Annot annotationMode
    , toArgs_Input inputType]
  ++ toArgs_userModules userModules
  where
    toArgs_userModules = map (("-u " ++) . UserModule.toString)
      . filter (/= userModule_Main)

    toArgs_Op Time = "--time"
    toArgs_Op Alloc = "--alloc"

    toArgs_Source SourceAlways = "-S"
    toArgs_Source SourceUser = "-s"
    toArgs_Source SourceNever = "--no-source"

    toArgs_FunctionName QualifiedAlways = "-Q"
    toArgs_FunctionName QualifiedKernel = "-q"
    toArgs_FunctionName QualifiedNever = "--no-qualified"

    toArgs_Annot WithAnnotations = "-A"
    toArgs_Annot NoAnnotations = "--no-annot"

    toArgs_Input Standard = "-p"
    toArgs_Input Detailed = "-P"

userModuleFromString :: String -> UserModule
userModuleFromString = fromRight' . UserModule.parseUserModule