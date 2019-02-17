{-# LANGUAGE ScopedTypeVariables #-}
module System.Terminal.Replique.Readline where

import qualified Control.Exception                as E
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           System.Terminal
import           Data.Proxy
import           Data.Char
import qualified Data.List.NonEmpty               as NE
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text                        as T
import           Data.Text.Prettyprint.Doc    hiding (width)
import           Prelude                      hiding (putChar)

import           System.Terminal
import           System.Terminal.Replique.Monad
import           System.Terminal.Replique.Image
import           System.Terminal.Replique.Readable
import           System.Terminal.Replique.RepliqueT

data ReadLineState m
    = ReadLineState 
    { rlWindow        :: Size
    , rlPosition      :: Position
    , rlPrompt        :: SimpleDocStream (Attribute m)
    , rlInputString   :: String
    , rlInputCursor   :: Int
    }

rlInputBeginPosition :: ReadLineState m -> Position
rlInputBeginPosition st = posInputBegin
    where
        size            = rlWindow st
        pos             = rlPosition st
        posInputBegin   = posPlusSimpleDocStream size pos (rlPrompt st)

rlInputEndPosition :: ReadLineState m -> Position
rlInputEndPosition st = posInputEnd
    where
        size            = rlWindow st
        pos             = rlPosition st
        posInputBegin   = rlInputBeginPosition st
        posInputEnd     = posPlus size posInputBegin $ length $ rlInputString st

putReadLineStateDiff :: MonadTerminal m => ReadLineState m -> ReadLineState m -> m (ReadLineState m)
putReadLineStateDiff oldSt newSt = putReadLineState newSt -- TODO

putReadLineState :: MonadTerminal m => ReadLineState m -> m (ReadLineState m)
putReadLineState st = do
    hideCursor
    setCursorPosition pos
    eraseInLine EraseAll
    eraseInDisplay EraseForward
    putSimpleDocStream (rlPrompt st)
    putString (rlInputString st)
    putChar ' ' -- overcome auto-wrap lazyness
    showCursor
    flush
    setCursorPosition posInputCursor'
    pure st { rlPosition = pos' }
    where
        size            = rlWindow st
        pos             = rlPosition st
        posInputStart   = posPlusSimpleDocStream size pos (rlPrompt st)
        posInputEnd     = posPlus size posInputStart (length $ rlInputString st)
        posInputCursor  = posPlus size posInputStart (rlInputCursor st)
        scrolled        = height size <= row posInputEnd
        pos'
            | scrolled  = posPlusRows size pos (-1)
            | otherwise = pos
        posInputCursor'
            | scrolled  = posPlusRows size posInputCursor (-1)
            | otherwise = posInputCursor

readLine :: forall m a. (MonadReplique m, Readable a) => Doc (Attribute m) -> m (Maybe a)
readLine prompt = do
    window <- getWindowSize
    cursor <- do
        pos <- getCursorPosition
        if col pos == 0
            then pure pos
            else putLn >> pure (Position (row pos + 1) 0)
    continue ReadLineState
        { rlWindow        = window
        , rlPosition      = cursor
        , rlPrompt        = layoutSmart defaultLayoutOptions (prompt <> space)
        , rlInputString   = ""
        , rlInputCursor   = 0
        }
    where
        prompt' = prompt <> space
        continue st = do
            st' <- putReadLineState st
            flush
            run st'
        quitWith st result = do
            setCursorPosition (rlInputEndPosition st)
            putLn
            flush
            pure result
        run st = awaitEvent >>= \case
            Left Interrupt -> interrupt
            Right ev -> case ev of
                KeyEvent EnterKey _ ->
                    case readText (T.pack $ rlInputString st) of
                        Left failure -> undefined -- FIXME
                        Right result -> quitWith st (Just result)
                KeyEvent (CharKey c) mods
                    | c == 'A' && mods == ctrlKey ->
                        continue (moveToLineBegin st)
                    | c == 'E' && mods == ctrlKey ->
                        continue (moveToLineEnd st)
                    | c == 'D' && mods == ctrlKey ->
                        quitWith st Nothing
                    | c == 'L' && mods == ctrlKey ->
                        continue (clear st)
                    | mods == mempty ->
                        continue (insertChar c st)
                    | otherwise -> do
                        continue st
                KeyEvent DeleteKey _
                    -> continue (delete st)
                KeyEvent BackspaceKey _
                    -> continue (deleteBackward st)
                KeyEvent (ArrowKey Leftwards) mods
                    | mods == ctrlKey -> continue (moveLeftWord st)
                    | otherwise       -> continue (moveLeft st)
                KeyEvent (ArrowKey Rightwards) mods
                    | mods == ctrlKey -> continue (moveRightWord st)
                    | otherwise       -> continue (moveRight st)
                _ -> continue st

clear :: ReadLineState m -> ReadLineState m
clear st = st
    { rlPosition = Position 0 0
    }

insertChar :: Char -> ReadLineState m -> ReadLineState m
insertChar c st = st
    { rlInputString = insertAt c (rlInputCursor st) (rlInputString st)
    , rlInputCursor = rlInputCursor st + 1
    }
    where
        insertAt x 0 ys     = x : ys
        insertAt x _ []     = []
        insertAt x i (y:ys) = y : insertAt x (i - 1) ys

delete :: ReadLineState m -> ReadLineState m
delete st = st
    { rlInputString = deleteAt (rlInputCursor st) (rlInputString st)
    }
    where
        deleteAt _     [] = []
        deleteAt 0 (_:xs) = xs
        deleteAt i (x:xs) = x : deleteAt (i - 1) xs 

deleteBackward :: ReadLineState m -> ReadLineState m
deleteBackward st 
    | rlInputCursor st == 0 = st
    | otherwise = st
        { rlInputString = deleteAt (rlInputCursor st - 1) (rlInputString st)
        , rlInputCursor = rlInputCursor st - 1
        }
    where
        deleteAt _     [] = []
        deleteAt 0 (_:xs) = xs
        deleteAt i (x:xs) = x : deleteAt (i - 1) xs 

moveLeft :: ReadLineState m -> ReadLineState m
moveLeft st = st
    { rlInputCursor = max 0 (rlInputCursor st - 1)
    }

moveLeftWord :: ReadLineState m -> ReadLineState m
moveLeftWord st = st
    { rlInputCursor = f 0 0 (rlInputString st)
    }
    where
        c = rlInputCursor st
        f _ l []         = l
        f i l (' ':x:xs)
            | i + 1 >= c = l
            | x /= ' '   = f (i + 2) (i + 1) xs
            | otherwise  = f (i + 2)  l      xs
        f i l (x:xs)
            | i >= c     = l
            | otherwise  = f (i + 1)  l      xs

moveRight :: ReadLineState m -> ReadLineState m
moveRight st = st
    { rlInputCursor = min (rlInputCursor st + 1) (length (rlInputString st))
    }

moveRightWord :: ReadLineState m -> ReadLineState m
moveRightWord st = st
    { rlInputCursor = f c (drop c xs)
    }
    where
        c  = rlInputCursor st
        xs = rlInputString st
        f c [] = c
        f c (x:xs)
            | x == ' '  = g (c + 1) xs
            | otherwise = f (c + 1) xs
        g c [] = c
        g c (x:xs)
            | x == ' '  = g (c + 1) xs
            | otherwise = c

moveToLineBegin :: ReadLineState m -> ReadLineState m
moveToLineBegin st = st
    { rlInputCursor = 0
    }

moveToLineEnd :: ReadLineState m -> ReadLineState m
moveToLineEnd st = st
    { rlInputCursor = length (rlInputString st)
    }

---------------------------------------------------------------------------------------------------
-- POSITION CALCULATIONS
---------------------------------------------------------------------------------------------------

posPlus :: Size -> Position -> Int -> Position
posPlus size pos i = Position r c
    where
        (r,c) = quotRem (row pos * width size + col pos + i) (width size)

posPlusRows :: Size -> Position -> Int -> Position
posPlusRows size (Position r c) i =
    Position (r + i) c

posPlusText :: Size -> Position -> T.Text -> Position
posPlusText size pos t = posPlus size pos (T.length t)

posPlusDoc :: Size -> Position -> Doc ann -> Position
posPlusDoc size pos doc = posPlusSimpleDocStream size pos ds
    where 
        ds = layoutSmart defaultLayoutOptions doc

posPlusSimpleDocStream :: Size -> Position -> SimpleDocStream ann -> Position
posPlusSimpleDocStream size pos0 ds0 = f ds0 pos0
    where
        f SFail           p = p
        f SEmpty          p = p
        f (SChar _ ds)    p = f ds $! posPlus size p 1
        f (SText i _ ds)  p = f ds $! posPlus size p i
        f (SLine i ds)    p = f ds $! posPlus size (Position (row p + 1) (col p)) i
        f (SAnnPush _ ds) p = f ds p
        f (SAnnPop ds)    p = f ds p
