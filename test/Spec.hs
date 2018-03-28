{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.State
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Text.Prettyprint.Doc

import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Monad.Replique
import           Control.Monad.Replique.Monad
import           Control.Monad.Terminal

main :: IO ()
main = defaultMain $
  testGroup "Control.Monad.Replique"
  [ testGroup "readLine"
    [ testReadLine0001
    ]
  ]

testReadLine0001 :: TestTree
testReadLine0001 = testCase "shall write the prompt at the right position" test
  where
    test = traceReplique input output (8,3) (4,0) $ do
      readLine (pretty prompt) :: TraceReplique T.Text
      pure ()
    input = [
        KeyEvent EnterKey mempty
      ]
    output = [
        HideCursor
      , SetCursorPosition (4,0)
      , ClearLine
      , ClearScreenBelow
      , PutSimpleDocStream $ layoutSmart (LayoutOptions Unbounded) (pretty prompt <+> space)
      , SetCursorPosition (5,3)
      , ShowCursor
      , HideCursor
      , PutChar '\n'
      , ShowCursor
      , AddToHistory ""
      ]
    prompt = "abc %" :: String

testReadLine0002 :: TestTree
testReadLine0002 = testCase "shall print an additonal newline if the carriage is not returned" test
  where
    test = traceReplique input output (8,3) (4,1) $ do
      readLine (pretty prompt) :: TraceReplique T.Text
      pure ()
    input = [
        KeyEvent EnterKey mempty
      ]
    output = [
        PutChar '\n'
      , HideCursor
      , SetCursorPosition (5,0)
      , ClearLine
      , ClearScreenBelow
      , PutSimpleDocStream $ layoutSmart (LayoutOptions Unbounded) (pretty prompt <+> space)
      , SetCursorPosition (6,3)
      , ShowCursor
      , HideCursor
      , PutChar '\n'
      , ShowCursor
      , AddToHistory ""
      ]
    prompt = "abc %" :: String

newtype TraceReplique a = TraceReplique (StateT TraceRepliqueState IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadQuit TraceReplique where
  quit = fail ""

instance MonadHistory TraceReplique where
  addToHistory = write . AddToHistory
  searchHistory = undefined

instance MonadTerminal TraceReplique where
  getScreenSize = TraceReplique $ gets sz
  getCursorPosition = TraceReplique $ gets cp
  hideCursor = write HideCursor
  showCursor = write ShowCursor
  setCursorPosition = write . SetCursorPosition
  clearLine = write ClearLine
  clearScreenBelow = write ClearScreenBelow

instance MonadPrinter TraceReplique where
  putChar = write . PutChar

instance MonadColorPrinter TraceReplique where

instance MonadPrettyPrinter TraceReplique where
  data Annotation TraceReplique = Foobar
  putDoc = write . PutSimpleDocStream . layoutSmart (LayoutOptions Unbounded) . reAnnotate (const ())

instance MonadFormatPrinter TraceReplique where

instance MonadInput TraceReplique where
  waitMapInterruptAndEvents f = TraceReplique $ do
    st <- get
    put st { input = tail (input st) }
    case input st of
      []    -> error "END OF INPUT"
      (e:_) -> liftIO $ atomically $ f retry (pure e)

data TraceRepliqueState
  = TraceRepliqueState
  { input  :: [Event]
  , output :: [Output]
  , sz     :: (Int,Int)
  , cp     :: (Int,Int)
  }

data Output
  = PutChar Char
  | PutSimpleDocStream (SimpleDocStream ())
  | HideCursor
  | ShowCursor
  | SetCursorPosition (Int,Int)
  | ClearLine
  | ClearScreenBelow
  | AddToHistory T.Text
  deriving (Eq, Show)

write :: Output -> TraceReplique ()
write cmd = TraceReplique $
  modify (\st-> st { output = output st ++ [cmd] })

traceReplique :: [Event] -> [Output] -> (Int,Int) -> (Int,Int) -> TraceReplique () ->Assertion
traceReplique input expectedOutput sz cp (TraceReplique ma) = do
  st' <- execStateT ma st
  expectedOutput @=? output st'
  where
    st = TraceRepliqueState input [] sz cp


data Screen
  = Screen
  { screenWidth     :: Int
  , screenHeight    :: Int
  , screenCursorRow :: Int
  , screenCursorCol :: Int
  , screenLines     :: [String]
  } deriving (Eq, Ord, Show)

sPutChar :: Char -> Screen -> Screen
sPutChar x s = s {
    screenCursorRow = cursorRow'
  , screenCursorCol = cursorCol'
  , screenLines     = lines'
  }
  where
    cursorCol'
      | eol       = 0
      | otherwise = screenCursorCol s + 1
    cursorRow'
      | eos       = screenCursorRow s
      | eol       = screenCursorRow s + 1
      | otherwise = screenCursorRow s
    eol = screenCursorCol s + 1 == screenWidth s
    eos = eol && screenCursorRow s + 1 == screenHeight s
    lines'
      | eos       = set $ tail (screenLines s)
      | otherwise = set $ screenLines s
    set ls = setRow ls (screenCursorRow s) (screenCursorCol s)
      where
        setRow []     0 c = [ setCol "" c ]
        setRow (l:ls) 0 c = setCol  l c : ls
        setRow []     r c = "" : setRow ls (r - 1) c
        setRow (l:ls) r c =  l : setRow ls (r - 1) c
        setCol ""       0 = [ x ]
        setCol (y:ys)   0 = x : ys
        setCol ""       c = ' ' : setCol "" (c - 1)
        setCol (y:ys)   c =   y : setCol ys (c - 1)

sSetCursorPosition :: (Int,Int) -> Screen -> Screen
sSetCursorPosition (row,col) s
  | row < 0 || row >= screenHeight s = error "out of bounds"
  | col < 0 || col >= screenWidth  s = error "out of bounds"
  | otherwise = s { screenCursorRow = row, screenCursorCol = col }

sClearLine :: Screen -> Screen
sClearLine  = s { screenLines = lines' }
  where
    lines' = f (screenCursorRow s) (screenLines s)
    f 0 []     = []
    f 0 (l:ls) = "" : ls
    f r []     = "" : f (r - 1) []
    f r (l:ls) =  l : f (r - 1) ls

sClearScreenBelow :: Screen -> Screen
sClearScreenBelow  = s { screenLines = lines' }
  where
    lines' = f (screenCursorRow s) (screenLines s)
    f 0 []     = []
    f 0 (l:ls) = [l]
    f r []     = []
    f r (l:ls) = l : f (r - 1) ls
