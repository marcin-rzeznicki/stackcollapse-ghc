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

