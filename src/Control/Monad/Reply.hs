{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Control.Monad.Reply (
    MonadQuit (..)
  , MonadStateful (..)
  , ReplyT ()
  , lift
  , runReplyT
  , readLine
  ) where

import qualified Control.Exception          as E
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Data.Char
import           Data.IORef
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Text.Prettyprint.Doc
import           Data.Typeable
import           Prelude                    hiding (putChar)
import qualified Text.Megaparsec            as P

import           Control.Monad.Reply.ReplyT
import           Control.Monad.Terminal

class Readable a where
  parse  :: String -> Maybe a
  pprint :: MonadColorPrinter m => a -> Doc (Annotation m)

instance Readable T.Text where
  parse = Just . T.pack
  pprint t = annotate (foreground $ bright Red) $ pretty t

readLine :: (MonadQuit m, MonadTerminal m, Readable a) => Doc (Annotation m) -> m a
readLine p = do
    resetAnnotations
    putDoc p
    flush
    withStacks [] []
    where
      withStacks xss yss = waitEvent >>= \case
        -- On Ctrl-C most shells just show a new prompt in the next line.
        -- This is probably to not terminate the program after pressing
        -- it several times when trying to interrupt a running computation.
        InterruptEvent -> do
          putLn
          readLine p
        -- On Ctrl-D the program is supposed to quit.
        KeyEvent (CharKey 'D') mods
          | mods == ctrlKey -> do
              putLn
              flush
              quit
        -- On Enter this function returns the entered string to the caller.
        KeyEvent EnterKey mods
          | mods == mempty -> case parse (reverse xss ++ yss) of
              Nothing -> withStacks xss yss
              Just a -> do
                putLn
                flush
                pure a
        KeyEvent BackspaceKey mods
          | mods == mempty -> case xss of
              []     -> withStacks xss yss
              (x:xs) -> do
                moveCursorBackward 1
                putString yss
                putChar ' '
                moveCursorBackward (length yss + 1)
                flush
                withStacks xs yss
        KeyEvent (ArrowKey Leftwards) mods
          | mods == mempty -> case xss of
              []     -> withStacks xss yss
              (x:xs) -> do
                moveCursorBackward 1
                flush
                withStacks xs (x:yss)
        KeyEvent (ArrowKey Rightwards) mods
          | mods == mempty -> case yss of
              []     -> withStacks xss yss
              (y:ys) -> do
                moveCursorForward 1
                flush
                withStacks (y:xss) ys
        KeyEvent (CharKey c) mods
          | mods == mempty && (isPrint c || isSpace c) -> do
              putChar c
              flush
              withStacks (c:xss) yss
          | otherwise ->
              withStacks xss yss
        ev -> do
          --putStringLn (show ev)
          flush
          withStacks xss yss
