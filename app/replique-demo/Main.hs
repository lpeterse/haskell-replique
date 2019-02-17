module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import qualified Control.Exception           as E
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class
import           Data.Char
import           Data.Function               (fix)
import qualified Data.Text                   as T
import qualified Data.Text                   as Text
import qualified Data.Text.Prettyprint.Doc   as PP
import qualified System.IO.Error             as E

import           System.Terminal
import           System.Terminal.Replique
import           Data.Text.Prettyprint.Doc

main :: IO ()
main = runReplique (255 :: Int) $ handleExit $ readLine prompt >>= \case
    Nothing      -> exitWith ExitSuccess
    Just line    -> case line of
        ""           -> pure ()
        "exit"       -> exitWith ExitSuccess
        "exitWith"   -> exitWith $ ExitFailure 123
        "fail"       -> fail "abcdef"
        "failIO"     -> liftIO $ E.throwIO $ E.userError "Exception thrown in IO."
        "throwM"     -> throwM $ E.userError "Exception thrown in RepliqueT."
        "liftThrowM" -> lift $ throwM $ E.userError "Exception thrown within the monad transformer."
        "load"       -> get >>= putStringLn . show
        "inc"        -> get >>= put . succ
        "dec"        -> get >>= put . pred
       {- "loop"       -> readLine "Integer: " >>= \case
            Nothing -> pure ()
            Just i  -> forM_ [1..(i :: Int)] $ \j-> put j >> putString (' ':show j)
        "move"       -> readLine "Integer: " >>= \case
            Nothing -> pure ()
            Just i  -> moveCursorForward i >> flush >> liftIO (threadDelay 3000000) -}
        "finally"    -> fail "I am failing, I am failing.." `finally` putStringLn "FINALLY"
        "clear"      -> eraseInDisplay EraseAll
        "window"     -> getWindowSize >>= \p-> putStringLn (show p) >> flush
        "cursor"     -> getCursorPosition >>= \p-> putStringLn (show p) >> flush
        "home"       -> setCursorPosition $ Position 0 0
        "undefined"  -> undefined
        "normal"     -> setAlternateScreenBuffer False
        "alternate"  -> setAlternateScreenBuffer True
        _            -> putStringLn (show (line :: String))

prompt :: (MonadFormattingPrinter m, MonadColorPrinter m) => Doc (Attribute m)
prompt = annotate bold (annotate (foreground $ bright blue) "demo") <> "@replique %"

handleExit :: (MonadExit m, MonadColorPrinter m, ExitStatus m ~ ExitCode) => m () -> m ()
handleExit ma = ma `catchExit` \e -> do
    putDocLn $ annotate (foreground yellow) (pretty $ show e)
    exitWith e
