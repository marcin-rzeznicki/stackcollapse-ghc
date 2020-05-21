{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           Helpers
import           Config
import           StackCollapse
import           Test.Hspec
import           Test.Hspec.Golden
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.ByteString.Lazy.UTF8 (toString)
import           Data.Either.Extra (fromEither)
import           Control.Arrow (left)

path_ProfFilesDir :: String
path_ProfFilesDir = "test/prof_files/"

path_GoldenDir :: String
path_GoldenDir = "test/.golden"

path_ProfFile :: String -> FilePath
path_ProfFile stem = path_ProfFilesDir ++ stem ++ ".prof"

main :: IO ()
main = hspec
  $ do
    context "detailed prof"
      $ withConfigs
        "test1"
        [ defaultConfig
        , defaultConfig `prependUserModule` userModuleFromString "Lesson11"
        , defaultConfig { sourceMode = SourceUser }
            `prependUserModule` userModuleFromString "Lesson11"
        , defaultConfig { functionNameMode = QualifiedNever }
            `prependUserModule` userModuleFromString "Lesson11"
        , defaultConfig { operationMode = Alloc }
            `prependUserModule` userModuleFromString "Lesson11"]
    context "standard prof"
      $ withConfigs
        "test2"
        [ defaultConfig { inputType = Standard }
            `prependUserModule` userModuleFromString "Lesson11"
        , defaultConfig { inputType = Standard } { operationMode = Alloc }
            `prependUserModule` userModuleFromString "Lesson11"]
    context "unicode chars"
      $ stackCollapseTest "test3"
      $ defaultConfig { inputType = Standard
                      , functionNameMode = QualifiedNever
                      }

withConfigs :: String -> [StackCollapseConfig] -> Spec
withConfigs fileStem = mapM_ (stackCollapseTest fileStem)

stackCollapseTest :: String -> StackCollapseConfig -> Spec
stackCollapseTest fileStem opts = before (prepareTest fileStem opts)
  $ specify (toArgsString opts) runAndCompareStackCollapse
  where
    runAndCompareStackCollapse =
      compareWithGold fileStem opts . fromEither . left pack . collapseStack

compareWithGold
  :: String -> StackCollapseConfig -> Lazy.ByteString -> Golden Lazy.ByteString
compareWithGold stem opts bs =
  Golden { output = bs
         , encodePretty = toString
         , writeToFile = Lazy.writeFile
         , readFromFile = Lazy.readFile
         , directory = path_GoldenDir
         , testName
         }
  where
    testName = stem ++ embedInFileName opts

prepareTest :: String -> StackCollapseConfig -> IO StackCollapse
prepareTest stem opts = let profFilePath = path_ProfFile stem
                        in stackCollapseFromPath opts profFilePath



