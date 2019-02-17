{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
module System.Terminal.Replique.RepliqueT where

import qualified Control.Exception             as E
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State hiding (get, put)
import           Control.Monad.State.Class
import           Data.Char
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc
import           GHC.Clock
import qualified System.Exit                   as System
import qualified System.IO.Error               as E
import           Prelude                   hiding (putChar)

import           System.Terminal
import           System.Terminal.Internal (LocalTerminal)
import           System.Terminal.Replique.Monad

---------------------------------------------------------------------------------------------------
-- PUBLIC
---------------------------------------------------------------------------------------------------

type Replique s = RepliqueT System.ExitCode s (TerminalT LocalTerminal IO) ()

runReplique :: s -> Replique s -> IO ()
runReplique s r = do
    e <- withTerminal (runTerminalT $ runRepliqueT r s)
    System.exitWith e

data Escape e
    = Exit e
    | Interrupted

newtype RepliqueT e s m a
    = RepliqueT { unRepliqueT :: ExceptT (Escape e) (StateT (RepliqueState s) m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

runRepliqueT  :: (MonadCatch m, MonadTerminal m) => RepliqueT e s m () -> s -> m e
runRepliqueT (RepliqueT m) ust = loop rst 0
    where
        rst = RepliqueState [] ust
        loop st timePreviousIteration = do
            timeNow <- liftIO getMonotonicTimeNSec
            if timeNow - timePreviousIteration < deltaMin
                then do
                    -- If the time between two loop iterations is below a certain
                    -- threshold it is assumed that the handler function is faulty
                    -- and would loop without user interaction. If it nonetheless
                    -- resets the iterrupt flag the user will not be able to
                    -- to stop the execution. An exception will be thrown in this case.
                    report renderInfiniteLoop
                    liftIO (E.throwIO $ E.userError "Replique: Application seems non-blocking")
                else handleExceptions st (runStateT (runExceptT m) st) >>= \case
                    (Left Interrupted, st') -> report renderInterrupt >> loop st' timeNow
                    (Left (Exit e), _)      -> pure e
                    (Right (), st')         -> loop st' timeNow
        handleExceptions st = handleAll $ \e -> do
            report (renderException e)
            pure (Right (), st)
        report d = do
            pos <- getCursorPosition
            when (col pos /= 0) putLn
            resetAttributes
            putDocLn d
            flush
        template x y =
            annotate (foreground red) $ "*** " <> x <> if null y
            then mempty else  ": " <> annotate (foreground $ bright red) (pretty (y :: String))
        renderInfiniteLoop = template "Infinite loop" "Application seems non-blocking"
        renderInterrupt    = template "Interrupted" ""
        renderException e  = template "Exception" $ E.displayException e
        deltaMin = 200000000 -- 200ms

--------------------------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------------------------

type instance (ExitStatus (RepliqueT e s m)) = e

data RepliqueState s
    = RepliqueState
    { repliqueHistory   :: [T.Text]
    , repliqueUserState :: s
    }

instance MonadTrans (RepliqueT e s) where
    lift ma = RepliqueT (lift (lift ma))

instance (Monad m) => MonadState s (RepliqueT e s m) where
    get   = RepliqueT (repliqueUserState <$> get)
    put s = RepliqueT (get >>= \r -> put r { repliqueUserState = s })

instance (MonadCatch m, MonadTerminal m) => MonadPrinter (RepliqueT e s m) where
    putLn        = interruptible $ lift putLn
    putChar      = interruptible . lift . putChar
    putString    = interruptible . lift . putString
    putStringLn  = interruptible . lift . putStringLn
    putText      = interruptible . lift . putText
    putTextLn    = interruptible . lift . putTextLn
    flush        = interruptible $ lift flush
    getLineWidth = interruptible $ lift getLineWidth

instance (MonadCatch m, MonadTerminal m) => MonadMarkupPrinter (RepliqueT e s m) where
    data Attribute (RepliqueT e s m) = Attribute' (Attribute m)
    setAttribute   (Attribute' a)    = interruptible $ lift (setAttribute a)
    resetAttribute (Attribute' a)    = interruptible $ lift (resetAttribute a)
    resetAttributes                  = interruptible $ lift  resetAttributes
    resetsAttribute (Attribute' a) (Attribute' b) = resetsAttribute a b

instance (MonadCatch m, MonadTerminal m) => MonadFormattingPrinter (RepliqueT e s m) where
    bold       = Attribute' bold
    italic     = Attribute' italic
    underlined = Attribute' underlined
    inverted   = Attribute' inverted

instance (MonadCatch m, MonadTerminal m) => MonadColorPrinter (RepliqueT e s m) where
    data Color (RepliqueT e s m) = Color'     (Color      m)
    bright     (Color' c)        = Color'     (bright     c)
    foreground (Color' c)        = Attribute' (foreground c)
    background (Color' c)        = Attribute' (background c)
    black                        = Color'      black
    red                          = Color'      red
    green                        = Color'      green
    yellow                       = Color'      yellow
    blue                         = Color'      blue
    magenta                      = Color'      magenta
    cyan                         = Color'      cyan
    white                        = Color'      white

instance (MonadCatch m, MonadTerminal m) => MonadScreen (RepliqueT e s m) where
    getWindowSize               = interruptible $ lift   getWindowSize

    getCursorPosition           = interruptible $ lift   getCursorPosition
    setCursorPosition           = interruptible . lift . setCursorPosition

    moveCursorUp                = interruptible . lift . moveCursorUp
    moveCursorDown              = interruptible . lift . moveCursorDown
    moveCursorBackward          = interruptible . lift . moveCursorBackward
    moveCursorForward           = interruptible . lift . moveCursorForward

    saveCursor                  = interruptible $ lift   saveCursor
    restoreCursor               = interruptible $ lift   restoreCursor

    showCursor                  = interruptible $ lift   showCursor
    hideCursor                  = interruptible $ lift   hideCursor

    insertChars                 = interruptible . lift . insertChars
    deleteChars                 = interruptible . lift . deleteChars
    insertLines                 = interruptible . lift . insertLines
    deleteLines                 = interruptible . lift . deleteLines

    eraseInLine                 = interruptible . lift . eraseInLine
    eraseInDisplay              = interruptible . lift . eraseInDisplay

    setAutoWrap                 = interruptible . lift . setAutoWrap
    setAlternateScreenBuffer    = interruptible . lift . setAlternateScreenBuffer

instance (MonadCatch m, MonadTerminal m) => MonadTerminal (RepliqueT e s m) where

instance (MonadCatch m, MonadInput m) => MonadInput (RepliqueT e s m) where
    awaitWith = lift . awaitWith

instance (Monad m) => MonadExit (RepliqueT e s m) where
    exitWith e = RepliqueT $ ExceptT $ pure $ Left (Exit e)
    catchExit (RepliqueT m) h = do
        eit <- RepliqueT $ ExceptT do
            runExceptT m >>= \case
                Left Interrupted -> pure (Left Interrupted)
                Left (Exit e)    -> pure (Right (Left  e))
                Right a          -> pure (Right (Right a))
        case eit of
            Left  e -> h e
            Right a -> pure a

instance (Monad m) => MonadHistory (RepliqueT e s m) where
    addToHistory t = RepliqueT do
        st <- get
        put st { repliqueHistory = f (repliqueHistory st) }
        where
        f history
            | not (T.all isSpace t) = t : history
            | otherwise             = history
    searchHistory t = RepliqueT do
        repliqueHistory <$> get

instance (MonadMask m, MonadTerminal m) => MonadReplique (RepliqueT e s m) where

interruptible :: MonadInput m => RepliqueT e s m a -> RepliqueT e s m a
interruptible ma = do
    interrupted <- lift checkInterrupt
    when interrupted $ RepliqueT $ ExceptT $ pure (Left Interrupted)
    ma

instance (Monad m) => MonadInterrupt (RepliqueT e s m) where
    interrupt = RepliqueT $ ExceptT $ pure (Left Interrupted)
    catchInterrupt = undefined