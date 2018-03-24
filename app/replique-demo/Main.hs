{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import qualified Control.Exception           as E
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Char
import           Data.Function               (fix)
import qualified Data.Text                   as T
import qualified Data.Text                   as Text
import qualified Data.Text.Prettyprint.Doc   as PP

import           Control.Monad.Replique
import           Control.Monad.Replique.IO
import           Control.Monad.Terminal
import           Data.Text.Prettyprint.Doc
import qualified System.IO.Error             as E
import           System.Terminal

main :: IO ()
main = withTerminal (runTerminalT $ runRepliqueT repl 0) >>= Prelude.print

prompt :: (MonadFormatPrinter m, MonadColorPrinter m) => Doc (Annotation m)
prompt = annotate bold $ annotate (foreground $ bright Blue) "replique" <> "@terminal % "

repl :: (MonadTerminal m, MonadColorPrinter m, MonadMask m, MonadIO m) => RepliqueT Int m ()
repl = readLine prompt >>= \case
    ""           -> pure ()
    "quit"       -> quit
    "fail"       -> fail "abcdef"
    "failIO"     -> liftIO $ E.throwIO $ E.userError "Exception thrown in IO."
    "throwM"     -> throwM $ E.userError "Exception thrown in RepliqueT."
    "liftThrowM" -> lift $ throwM $ E.userError "Exception thrown within the monad transformer."
    "load"       -> load >>= pprint
    "inc"        -> load >>= store . succ
    "dec"        -> load >>= store . pred
    "loop"       -> forM_ [1..100000] $ \i-> store i >> putString (' ':show i)
    "finally"    -> fail "I am failing, I am failing.." `finally` putStringLn "FINALLY"
    "clear"      -> clearScreen
    "screen"     -> getScreenSize >>= \p-> putStringLn (show p) >> flush
    "cursor"     -> getCursorPosition >>= \p-> putStringLn (show p) >> flush
    "home"       -> setCursorPosition (0,0)
    "progress"   -> void $ runWithProgressBar $ \update-> (`finally` threadDelay 3000000) $ forM_ [1..1000] $ \i-> do
                      threadDelay 10000
                      update $ fromIntegral i / 1000
    "colors"     -> undefined
    "normal"     -> useAlternateScreenBuffer False
    "alternate"  -> useAlternateScreenBuffer True
    line         -> putStringLn (show (line :: T.Text))
