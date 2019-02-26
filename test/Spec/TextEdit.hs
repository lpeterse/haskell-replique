module Spec.TextEdit (tests) where

import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad.Trans.State
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Text.Prettyprint.Doc

import           Test.Tasty
import           Test.Tasty.HUnit

import           System.Terminal
import           System.Terminal.Internal
import           System.Terminal.Replique.TextEdit

tests :: TestTree
tests = testGroup "TextEdit"
  [ testGroup "render"
    [ testRender01
    , testRender02
    , testRender03
    , testRender04
    , testRender05
    , testRender06
    , testRender07
    , testRender08
    , testRender09
    ]
  , testGroup "diff"
    [ testDiff01
    , testDiff02
    , testDiff03
    , testDiff04
    , testDiff05
    , testDiff06
    , testDiff07
    , testDiff08
    , testDiff09
    ]
  , testGroup "putDiff"
    [ testPutDiff01
    , testPutDiff02
    , testPutDiff03
    , testPutDiff04
    , testPutDiff05
    , testPutDiff06
    , testPutDiff07
    ]
  ]

testRender01 :: TestTree
testRender01 = testCase "shall return unmodified when enough space available" do
    assertEqual "" ("0123456789", 5) $ render 10 ("0123456789", 5)

testRender02 :: TestTree
testRender02 = testCase "shall truncate right when cursor is near left margin" do
    assertEqual "" ("01 ..", 1) $ render 5 ("0123456789", 1)

testRender03 :: TestTree
testRender03 = testCase "shall truncate left when cursor is near right margin" do
    assertEqual "" (".. CDEF", 3) $ render 7 ("0123456789ABCDEF", 12)

testRender04 :: TestTree
testRender04 = testCase "shall truncate both when cursor is not close enough to the left" do
    assertEqual "" (".. 4 ..", 3) $ render 7 ("0123456789ABCDEF", 4)

testRender05 :: TestTree
testRender05 = testCase "shall truncate both when cursor is not close enough to the right" do
    assertEqual "" (".. B ..", 3) $ render 7 ("0123456789ABCDEF", 11)

testRender06 :: TestTree
testRender06 = testCase "shall truncate both when cursor is centered (available is odd)" do
    assertEqual "" (".. 678 ..", 4) $ render 9 ("0123456789ABCDEF", 7)

testRender07 :: TestTree
testRender07 = testCase "shall truncate both when cursor is centered (available is even)" do
    assertEqual "" (".. 5678 ..", 5) $ render 10 ("0123456789ABCDEF", 7)

testRender08 :: TestTree
testRender08 = testCase "shall add extra whitespace when cursor is on mostright position" do
    assertEqual "" ("0123456789 ", 10) $ render 11 ("0123456789", 10)

testRender09 :: TestTree
testRender09 = testCase "shall return whitespace when string is empty" do
    assertEqual "" (" ", 0) $ render 10 ("", 0)

testDiff01 :: TestTree
testDiff01 = testCase "empty strings" do
    assertEqual "" ("",("",""),"") (diff "" "")

testDiff02 :: TestTree
testDiff02 = testCase "completely different strings" do
    assertEqual "" ("",("abc","def"),"") (diff "abc" "def")

testDiff03 :: TestTree
testDiff03 = testCase "different substrings of equal length" do
    assertEqual "" ("123",("abc","def"),"567") (diff "123abc567" "123def567")

testDiff04 :: TestTree
testDiff04 = testCase "different substrings of different length" do
    assertEqual "" ("123",("abcde","def"),"567") (diff "123abcde567" "123def567")

testDiff05 :: TestTree
testDiff05 = testCase "insertion" do
    assertEqual "" ("123",("","def"),"567") (diff "123567" "123def567")

testDiff06 :: TestTree
testDiff06 = testCase "deletion" do
    assertEqual "" ("123",("def",""),"567") (diff "123def567" "123567")

testDiff07 :: TestTree
testDiff07 = testCase "equal strings" do
    assertEqual "" ("123456",("",""),"") (diff "123456" "123456")

testDiff08 :: TestTree
testDiff08 = testCase "equal prefix" do
    assertEqual "" ("123456",("","7"),"") (diff "123456" "1234567")

testDiff09 :: TestTree
testDiff09 = testCase "equal postfix" do
    assertEqual "" ("",("0",""),"123456") (diff "0123456" "123456")

testPutDiff01 :: TestTree
testPutDiff01 = testCase "replace 3 chars in first line" do
    (c,t) <- runVirtual size do
        let old = "01234567890123456789"
        putString old
        setCursorPosition pos0
        let new = "0___4567890123456789"
        putDiff size pos0 old new
    assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
    assertEqual "cursor (terminal)" expCursor =<< readTVarIO (virtualCursor t)
    assertEqual "cursor (putDiff)"  expCursor c
    where
        size      = Size 4 10
        pos0      = Position 0 0
        expCursor = Position 0 4
        expWindow =
            [ "0___456789"
            , "0123456789"
            , "          "
            , "          " ]

testPutDiff02 :: TestTree
testPutDiff02 = testCase "delete 3 chars in first line" do
    (c,t) <- runVirtual size do
        let old = "01234567890123456789"
        putString old
        setCursorPosition pos0
        let new = "04567890123456789"
        putDiff size pos0 old new
    assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
    assertEqual "cursor (terminal)" expCursor =<< readTVarIO (virtualCursor t)
    assertEqual "cursor (putDiff)"  expCursor c
    where
        size      = Size 4 10
        pos0      = Position 0 0
        expCursor = Position 1 7
        expWindow =
            [ "0456789012"
            , "3456789   "
            , "          "
            , "          " ]

testPutDiff03 :: TestTree
testPutDiff03 = testCase "delete 4 chars ranging from 1st to 2nd line" do
    (c,t) <- runVirtual size do
        let old = "01234567890123456789"
        putString old
        setCursorPosition pos0
        let new = "0123456723456789"
        putDiff size pos0 old new
    assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
    assertEqual "cursor (terminal)" expCursor =<< readTVarIO (virtualCursor t)
    assertEqual "cursor (putDiff)"  expCursor c
    where
        size      = Size 4 10
        pos0      = Position 0 0
        expCursor = Position 1 6
        expWindow =
            [ "0123456723"
            , "456789    "
            , "          "
            , "          " ]

testPutDiff04 :: TestTree
testPutDiff04 = testCase "delete 12 chars ranging from 1st to 3rd line" do
    (c,t) <- runVirtual size do
        let old = "012345678___________ABCD"
        putString old
        setCursorPosition pos0
        let new = "012345678ABCD"
        putDiff size pos0 old new
    assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
    assertEqual "cursor (terminal)" expCursor =<< readTVarIO (virtualCursor t)
    assertEqual "cursor (putDiff)"  expCursor c
    where
        size      = Size 4 10
        pos0      = Position 0 0
        expCursor = Position 1 3
        expWindow =
            [ "012345678A"
            , "BCD       "
            , "          "
            , "          " ]

testPutDiff05 :: TestTree
testPutDiff05 = testCase "insert 3 chars in first line" do
    (c,t) <- runVirtual size do
        let old = "01234567890123456789"
        putString old
        setCursorPosition pos0
        let new = "012___34567890123456789"
        putDiff size pos0 old new
    assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
    assertEqual "cursor (terminal)" expCursor =<< readTVarIO (virtualCursor t)
    assertEqual "cursor (putDiff)"  expCursor c
    where
        size      = Size 4 10
        pos0      = Position 0 0
        expCursor = Position 2 3
        expWindow =
            [ "012___3456"
            , "7890123456"
            , "789       "
            , "          " ]

testPutDiff06 :: TestTree
testPutDiff06 = testCase "insert 4 chars ranging from 1s to 2nd line" do
    (c,t) <- runVirtual size do
        let old = "01234567890123456789"
        putString old
        setCursorPosition pos0
        let new = "01234567____890123456789"
        putDiff size pos0 old new
    assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
    assertEqual "cursor (terminal)" expCursor =<< readTVarIO (virtualCursor t)
    assertEqual "cursor (putDiff)"  expCursor c
    where
        size      = Size 4 10
        pos0      = Position 0 0
        expCursor = Position 2 4
        expWindow =
            [ "01234567__"
            , "__89012345"
            , "6789      "
            , "          " ]

testPutDiff07 :: TestTree
testPutDiff07 = testCase "insert 12 chars ranging from 1s to 3rd line" do
    (c,t) <- runVirtual size do
        let old = "0123456789012345678901234"
        putString old
        setCursorPosition pos0
        let new = "012345678____________9012345678901234"
        putDiff size pos0 old new
    assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
    assertEqual "cursor (terminal)" expCursor =<< readTVarIO (virtualCursor t)
    assertEqual "cursor (putDiff)"  expCursor c
    where
        size      = Size 4 10
        pos0      = Position 0 0
        expCursor = Position 3 7
        expWindow =
            [ "012345678_"
            , "__________"
            , "_901234567"
            , "8901234   " ]

runVirtual :: Size -> TerminalT VirtualTerminal IO a -> IO (a, VirtualTerminal)
runVirtual size ma = withVirtualTerminal settings $ \t -> do
    a <- runTerminalT ma t
    pure (a, t)
    where
        settings = VirtualTerminalSettings
            { virtualType       = "xterm"
            , virtualWindowSize = pure size
            , virtualEvent      = retry
            , virtualInterrupt  = retry
            }

