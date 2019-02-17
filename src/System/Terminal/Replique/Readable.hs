module System.Terminal.Replique.Readable where

import           Data.Char
import           Data.Proxy
import qualified Data.Text                        as T
import           Text.Read                           (readMaybe)
import           Data.Text.Prettyprint.Doc
import           System.Terminal

class Readable a where
    readText :: T.Text -> Either T.Text a

instance Readable T.Text where
    readText t = Right t

instance Readable [Char] where
    readText t = Right (T.unpack t)
