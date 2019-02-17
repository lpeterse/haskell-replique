module System.Terminal.Replique.Readable where

import qualified Data.Text                        as T

class Readable a where
    readText :: T.Text -> Either T.Text a

instance Readable T.Text where
    readText t = Right t

instance Readable [Char] where
    readText t = Right (T.unpack t)
