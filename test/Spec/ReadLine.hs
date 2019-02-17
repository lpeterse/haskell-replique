module Spec.ReadLine (tests) where

import           Control.Concurrent.Async
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TChan
import           Control.Monad.Trans.State
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Text.Prettyprint.Doc

import           Test.Tasty
import           Test.Tasty.HUnit

import           System.Terminal
import           System.Terminal.Internal
import           System.Terminal.Replique

tests :: TestTree
tests = testGroup "ReadLine"
    [ testGroup "readLine"
        [ testReadLine01
        , testReadLine02
        , testReadLine03
        ]
    ]

testReadLine01 :: TestTree
testReadLine01 = testCase "shall print prompt and exit on Ctrl+D" do
    ev <- newTChanIO
    s <- newSettings size ev
    withVirtualTerminal s $ \t -> do
        let repl = do
                x <- readLine "prompt %"
                exitWith (x :: Maybe T.Text)
        withAsync (runTerminalT (runRepliqueT repl ()) t) $ \result -> do
            atomically $ writeTChan ev (KeyEvent (CharKey 'D') ctrlKey)
            assertEqual "return" Nothing   =<< wait result
            assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
            assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        size = Size 4 10
        expCursor = Position 1 0
        expWindow =
            [ "prompt %  "
            , "          "
            , "          "
            , "          " ]

testReadLine02 :: TestTree
testReadLine02 = testCase "shall accept a single character" do
    ev <- newTChanIO
    s <- newSettings size ev
    withVirtualTerminal s $ \t -> do
        let repl = do
                x <- readLine "prompt %"
                exitWith (x :: Maybe T.Text)
        withAsync (runTerminalT (runRepliqueT repl ()) t) $ \result -> do
            atomically $ writeTChan ev (KeyEvent (CharKey 'A') mempty)
            atomically $ writeTChan ev (KeyEvent (CharKey 'D') ctrlKey)
            assertEqual "return" Nothing   =<< wait result
            assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
            assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        size = Size 4 10
        expCursor = Position 2 0
        expWindow =
            [ "prompt % A"
            , "          "
            , "          "
            , "          " ]

testReadLine03 :: TestTree
testReadLine03 = testCase "shall accept and delete single character" do
    ev <- newTChanIO
    s <- newSettings size ev
    withVirtualTerminal s $ \t -> do
        let repl = do
                x <- readLine "prompt %"
                exitWith (x :: Maybe T.Text)
        withAsync (runTerminalT (runRepliqueT repl ()) t) $ \result -> do
            atomically $ writeTChan ev (KeyEvent (CharKey 'A') mempty)
            atomically $ writeTChan ev (KeyEvent BackspaceKey mempty)
            atomically $ writeTChan ev (KeyEvent (CharKey 'D') ctrlKey)
            assertEqual "return" Nothing   =<< wait result
            assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
            assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        size = Size 4 10
        expCursor = Position 1 0
        expWindow =
            [ "prompt %  "
            , "          "
            , "          "
            , "          " ]

newSettings :: Size -> TChan Event -> IO VirtualTerminalSettings
newSettings size ev = pure VirtualTerminalSettings
    { virtualType       = "xterm"
    , virtualWindowSize = pure size
    , virtualEvent      = readTChan ev
    , virtualInterrupt  = retry
    }
