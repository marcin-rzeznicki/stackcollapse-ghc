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
module Main (main) where

import           Config
import           UserModule
import           StackCollapse
import           System.Console.GetOpt
import           System.Environment (getArgs, getProgName)
import           System.Exit
import           System.IO (Handle, hPutStrLn, stderr, hPutChar, stdout)
import           Data.Functor ((<&>))
import           Data.Function ((&))
import           Control.Monad (foldM)
import           Control.Exception
import qualified Data.ByteString.Lazy as Lazy

cmdLineOpts
  :: [OptDescr (StackCollapseConfig -> Either String StackCollapseConfig)]
cmdLineOpts =
  [ Option
      ['P']
      []
      (NoArg $ \config -> Right config { inputType = Detailed })
      "Process detailed prof file ('-P' or '-pa' options) (default)"
  , Option
      ['p']
      []
      (NoArg $ \config -> Right config { inputType = Standard })
      "Process standard prof file ('-p' option)"
  , Option
      ['u']
      ["user-module"]
      (ReqArg
         (\input config -> parseUserModule input <&> prependUserModule config)
         "module_name")
      "Name of an user module (may be repeated to add more than one). Matches the whole \"hierarchy\", so `MyModule` matches `MyModule` as well as `MyModule.Internal`"
  , Option
      ['t']
      ["time"]
      (NoArg $ \config -> Right config { operationMode = Time })
      "Collapse with respect to time (default)"
  , Option
      ['a']
      ["alloc"]
      (NoArg $ \config -> Right config { operationMode = Alloc })
      "Collapse with respect to allocations"
  , Option
      ['S']
      []
      (NoArg $ \config -> Right config { sourceMode = SourceAlways })
      "Append source location to every function name"
  , Option
      ['s']
      []
      (NoArg $ \config -> Right config { sourceMode = SourceUser })
      "Append source location to functions defined in user modules"
  , Option
      []
      ["no-source"]
      (NoArg $ \config -> Right config { sourceMode = SourceNever })
      "Do not append source location (default)"
  , Option
      ['Q']
      []
      (NoArg $ \config -> Right config { functionNameMode = QualifiedAlways })
      "Always use qualified functon names"
  , Option
      []
      ["no-qualified"]
      (NoArg $ \config -> Right config { functionNameMode = QualifiedNever })
      "Do not use qualified function names"
  , Option
      ['q']
      []
      (NoArg $ \config -> Right config { functionNameMode = QualifiedKernel })
      "Use qualified names for functions not defined in user modules (default)"
  , Option
      ['A']
      ["annotations", "annot"]
      (NoArg $ \config -> Right config { annotationMode = WithAnnotations })
      "Add annotations to output (j - user modules, k - GHC and System, i - CAFs)"
  , Option
      []
      ["no-annotations", "no-annot"]
      (NoArg $ \config -> Right config { annotationMode = NoAnnotations })
      "Do not add annotations to output"]

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run [] = printUsage stdout
run ["-h"] = printUsage stdout
run ["--help"] = printUsage stdout
run args = case getOpt RequireOrder cmdLineOpts args of
  (opts, [path], []) -> do
    let configOrError = foldM (&) defaultConfig opts
    either
      invalidArgumentError
      ((`catch` runtimeError) . runStackCollapse path)
      configOrError
  (_, _, errors)     -> invalidUsageError errors

runStackCollapse :: FilePath -> StackCollapseConfig -> IO ()
runStackCollapse inpath config = stackCollapseFromPath config inpath
  <&> collapseStack
  >>= either (runtimeError . userError) Lazy.putStrLn

printUsage :: Handle -> IO ()
printUsage h = do
  exe <- getProgName
  hPutChar h '\n'
  hPutStrLn h
    $ usageInfo ("Usage: " ++ exe ++ " [OPTIONS] FILE\nOPTIONS:") cmdLineOpts
  hPutChar h '\n'

invalidUsageError :: [String] -> IO a
invalidUsageError errorMessages = do
  let errorMessage = concat errorMessages
  putErrLn errorMessage
  printUsage stderr
  exitFailure

invalidArgumentError :: String -> IO a
invalidArgumentError errorMessage = do
  putErrLn $ "Invalid argument: " ++ errorMessage
  exitFailure

runtimeError :: IOError -> IO a
runtimeError ioerror = do
  putErrLn $ displayException ioerror
  exitWith $ ExitFailure 2

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr



