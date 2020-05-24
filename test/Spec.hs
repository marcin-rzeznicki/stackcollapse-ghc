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
        "semiprimes"
        [ defaultConfig
        , defaultConfig `prependUserModule` userModuleFromString "Example"
        , defaultConfig { sourceMode = SourceUser }
            `prependUserModule` userModuleFromString "Example"
        , defaultConfig { functionNameMode = QualifiedNever }
            `prependUserModule` userModuleFromString "Example"
        , defaultConfig { operationMode = Alloc }
            `prependUserModule` userModuleFromString "Example"]
    context "standard prof"
      $ withConfigs
        "countSemiprimes"
        [ defaultConfig { inputType = Standard }
            `prependUserModule` userModuleFromString "Example"
        , defaultConfig { inputType = Standard, operationMode = Alloc }
            `prependUserModule` userModuleFromString "Example"]
    context "unicode chars"
      $ withConfigs
        "unicode"
        [ defaultConfig { inputType = Standard
                        , functionNameMode = QualifiedNever
                        }
        , defaultConfig { inputType = Standard
                        , operationMode = Alloc
                        , sourceMode = SourceAlways
                        }]
    context "no header" $ stackCollapseTest "no-header" defaultConfig
    context "just totals" $ stackCollapseTest "no-data" defaultConfig
    context "no totals"
      $ stackCollapseTest "no-totals"
      $ defaultConfig { inputType = Standard }
    context "malformed total bytes"
      $ stackCollapseTest "malformed-bytes"
      $ defaultConfig { inputType = Standard }
    context "malformed ticks"
      $ stackCollapseTest "no-ticks"
      $ defaultConfig { inputType = Standard }
    context "data errors"
      $ withConfigs
        "errors"
        [defaultConfig, defaultConfig { operationMode = Alloc }]

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



