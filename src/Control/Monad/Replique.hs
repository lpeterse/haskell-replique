{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Control.Monad.Replique (
    MonadQuit (..)
  , MonadStateful (..)
  , RepliqueT ()
  , lift
  , runRepliqueT
  , readLine
  ) where

import qualified Control.Exception                as E
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Data.Char
import           Data.IORef
import           Data.Maybe
import qualified Data.Text                        as T
import           Data.Text.Prettyprint.Doc
import           Data.Typeable

import           Control.Monad.Replique.RepliqueT
import           Control.Monad.Terminal

data ReadlineState m
  = ReadlineState
  { prompt     :: Doc (Annotation m)
  , inputLeft  :: String
  , inputRight :: String
  , screenSize :: (Int,Int)
  }

readLine :: (MonadQuit m, MonadTerminal m) => Doc (Annotation m) -> m T.Text
readLine p = do
  sz <- getScreenSize
  cp <- getCursorPosition
  -- Always print the prompt on the left side of the screen.
  -- Add an additional newline if necessary.
  when (snd cp /= 0) putLn
  saveCursorPosition
  run (ReadlineState p "" "" sz)
  where
    run st = waitEvent >>= \case
      -- On Ctrl-C most shells just show a new prompt in the next line
      -- without erasing what has been typed so far.
      InterruptEvent -> undefined
      -- On Ctrl-D the program is supposed to quit when the input is empty.
      -- Otherwise do nothing.
      KeyEvent (CharKey 'D') mods
        | mods == ctrlKey -> undefined
      -- On Enter this function returns the entered string to the caller.
      KeyEvent EnterKey mods
        | mods == mempty -> do
            hideCursor
            let (x,_) = relativeCursorPosition (snd $ screenSize st) (p <> pretty (reverse $ inputLeft st))
            let (y,_) = relativeCursorPosition (snd $ screenSize st) (p <> pretty (reverse $ inputLeft st) <> pretty (inputRight st))
            replicateM_ (1 + y - x) putLn
            showCursor
            flush
            pure (T.pack $ reverse (inputLeft st) ++ inputRight st)
      KeyEvent BackspaceKey mods
        | mods == mempty -> case inputLeft st of
            []     -> run st
            (l:ls) -> diff st st { inputLeft = ls }
      KeyEvent DeleteKey mods
        | mods == mempty -> case inputRight st of
            []     -> run st
            (r:rs) -> diff st st { inputRight = rs }
      KeyEvent (ArrowKey Leftwards) mods
        | mods == mempty -> case inputLeft st of
            []     -> run st
            (l:ls) -> diff st st { inputLeft = ls, inputRight = l : inputRight st }
      KeyEvent (ArrowKey Rightwards) mods
        | mods == mempty -> case inputRight st of
            []     -> run st
            (r:rs) -> diff st st { inputLeft = r : inputLeft st, inputRight = rs }
      KeyEvent (CharKey c) mods
        | mods == mempty && (isPrint c || isSpace c)
         -> diff st st { inputLeft = c : inputLeft st }
        | otherwise
         -> run st
      ev -> run st

    diff old new = do
      hideCursor
      restoreCursorPosition
      clearLine
      clearScreenBelow
      putDoc $ prompt new <> pretty (reverse $ inputLeft new) <> pretty (inputRight new)
      restoreCursorPosition
      let (r,c) = relativeCursorPosition (snd $ screenSize new) (prompt new <> pretty (reverse $ inputLeft new))
      when (r > 1) $ moveCursorDown (r - 1)
      when (c > 0) $ moveCursorRight c
      showCursor
      flush
      run new

relativeCursorPosition :: Int -> Doc ann -> (Int, Int)
relativeCursorPosition lineWidth doc = (length lineWidths, last lineWidths)
  where
    sds = layoutPretty (LayoutOptions (AvailablePerLine lineWidth 1.0)) doc
    lineWidths = f 0 sds
      where
        f w SFail           = [w]
        f w SEmpty          = [w]
        f w (SAnnPush _ ss) = f w ss
        f w (SAnnPop    ss) = f w ss
        f w (SChar    _ ss) = f (w + 1) ss
        f w (SLine    i ss) = w : f i ss
        f w (SText  l _ ss) = if a > 0
            then replicate a lineWidth ++ f b ss
            else f (w + l) ss
          where
            (a,b) = quotRem (w + l) lineWidth

