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
module UserModule (UserModule, parseUserModule, inAnyUserModule) where

import           Trace
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Char (isDigit, isLetter, isUpper)
import           Data.Maybe (listToMaybe)
import           Data.Coerce

newtype UserModule = UserModule Text

parseUserModule :: String -> Either String UserModule
parseUserModule string
  | not allLettersOrDigitsOrDotOr' =
    Left "Module name must consist of letter | digit | . | ' only"
  | not startsWithUpper = Left "Module name must start with uppercase letter"
  | endsWithDot = Left "Do not end module name with '.'"
  | otherwise = Right $ UserModule $ T.pack string
  where
    allLettersOrDigitsOrDotOr' =
      all (\c -> isLetter c || isDigit c || c == '\'' || c == '.') string

    startsWithUpper = maybe False isUpper $ listToMaybe string

    endsWithDot = last string == '.'

inAnyUserModule :: [UserModule] -> Trace -> Bool
inAnyUserModule = inAnyModule . coerce

