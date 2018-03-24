{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Control.Monad.Replique (
    MonadQuit (..)
  , MonadStateful (..)
  , RepliqueT ()
  , lift
  , runRepliqueT
  , readString
  , readText
  ) where

import qualified Control.Exception                as E
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Data.Char
import           Data.IORef
import qualified Data.List.NonEmpty               as NE
import           Data.Maybe
import qualified Data.Text                        as T
import           Data.Text.Prettyprint.Doc
import           Data.Typeable

import           Control.Monad.Replique.RepliqueT
import           Control.Monad.Terminal

data ReadlineState m
  = ReadlineState
  { inputLeft  :: String
  , inputRight :: String
  , lineNumber :: Int
  , screenSize :: (Int,Int)
  }

readText :: (MonadQuit m, MonadTerminal m) => Doc (Annotation m) -> m T.Text
readText prompt = T.pack <$> readString prompt

readString :: (MonadQuit m, MonadTerminal m) => Doc (Annotation m) -> m String
readString prompt = do
  sz <- getScreenSize
  line <- do
    (row,column) <- getCursorPosition
    -- Always print the prompt on the left side of the screen.
    -- Add an additional newline if necessary.
    if column /= 0
      then putLn >> pure (min (fst sz - 1) (row + 1)) -- Eventual scrolling on end of screen.
      else pure row
  let st = ReadlineState "" "" line sz
  update st
  run st
  where
    run st = waitEvent >>= \case
      -- On Ctrl-C most shells just show a new prompt in the next line
      -- without erasing what has been typed so far.
      InterruptEvent -> do
        finalize st
        readString prompt
      -- On Ctrl-D the program is supposed to quit when the input is empty.
      -- Otherwise do nothing.
      KeyEvent (CharKey 'D') mods
        | mods == ctrlKey && null (inputLeft st) && null (inputRight st) -> do
            finalize st
            quit
        | otherwise ->
            run st
      -- On Enter this function returns the entered string to the caller.
      KeyEvent EnterKey mods
        | mods == mempty -> do
            finalize st
            pure (reverse (inputLeft st) ++ inputRight st)
      KeyEvent BackspaceKey mods
        | mods == mempty -> case inputLeft st of
            []     -> run st
            (l:ls) -> update st { inputLeft = ls } >>= run
      KeyEvent DeleteKey mods
        | mods == mempty -> case inputRight st of
            []     -> run st
            (r:rs) -> update st { inputRight = rs } >>= run
      KeyEvent (ArrowKey Leftwards) mods
        | mods == mempty -> case inputLeft st of
            []     -> run st
            (l:ls) -> update st { inputLeft = ls, inputRight = l : inputRight st } >>= run
      KeyEvent (ArrowKey Rightwards) mods
        | mods == mempty -> case inputRight st of
            []     -> run st
            (r:rs) -> update st { inputLeft = r : inputLeft st, inputRight = rs } >>= run
      KeyEvent (CharKey c) mods
        | mods == mempty && (isPrint c || isSpace c)
         -> update st { inputLeft = c : inputLeft st } >>= run
        | otherwise
         -> run st
      ev -> run st

    update st = do
      hideCursor
      setCursorPosition promptPosition
      clearLine
      clearScreenBelow
      putDoc doc
      setCursorPosition cursorPosition'
      showCursor
      flush
      pure st { lineNumber = promptLine' }
      where
        height     = fst (screenSize st)
        width      = snd (screenSize st)
        promptLine = lineNumber st
        promptLine' -- Eventual scrolling is taken into account here.
          | promptLine + NE.length lws > height = max 0 (height - NE.length lws)
          | otherwise                           = promptLine
        promptPosition  = (promptLine, 0)
        cursorPosition' = (promptLine' + NE.length lwsToCursor - 1, NE.last lwsToCursor)
        -- The `space` at the end of the document assures that the terminal actually
        -- scrolls when the cursor reaches the bottom right corner: The last character
        -- shall be printed at the bottom right corner, then a scroll shall happen and the
        -- cursor shall then appear at the beginning of the next line.
        -- Always adding the `space` is not necessary, but it saves a conditional
        -- case distinction which makes the code less error prone.
        doc          = docToCursor <> pretty (inputRight st) <> space
        docToCursor  = prompt <> pretty (reverse $ inputLeft st)
        lws          = lineWidths width doc
        lwsToCursor  = lineWidths width docToCursor

    finalize st = do
      hideCursor
      replicateM_ (1 + NE.length lws - NE.length lwsToCursor) putLn
      showCursor
      flush
      where
        width        = snd (screenSize st)
        docc         = doccToCursor <> pretty (inputRight st) <> space
        doccToCursor = prompt <> pretty (reverse $ inputLeft st)
        lws          = lineWidths width docc
        lwsToCursor  = lineWidths width doccToCursor


lineWidths :: Int -> Doc ann -> NE.NonEmpty Int
lineWidths maxLineWidth doc = f 0 sds
  where
    f w SFail           = pure w
    f w SEmpty          = pure w
    f w (SAnnPush _ ss) = f w ss
    f w (SAnnPop    ss) = f w ss
    f w (SChar    _ ss) = f (w + 1) ss
    f w (SLine    i ss) = NE.cons w (f i ss)
    f w (SText  l _ ss) = g (f lastLineWidth ss) fullLines
      where
        (fullLines,lastLineWidth)  = quotRem (w + l) maxLineWidth
        g xs i -- prepends maxLineWidth i times
          | i > 0     = g (NE.cons maxLineWidth xs) (i - 1)
          | otherwise = xs
    sds = layoutPretty (LayoutOptions (AvailablePerLine maxLineWidth 1.0)) doc
