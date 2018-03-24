{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Control.Monad.Replique.RepliqueT (
    MonadQuit (..)
  , MonadStateful (..)
  , RepliqueT ()
  , lift
  , runRepliqueT
  ) where

import qualified Control.Exception            as E
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Data.Char
import           Data.IORef
import           Data.Maybe
import qualified Data.Text                    as T
import           Data.Text.Prettyprint.Doc
import           Data.Typeable

import           Control.Monad.Terminal
import qualified Control.Monad.Terminal       as T

import           Control.Monad.Replique.Monad

newtype Failure = Failure String
  deriving (Eq, Ord, Show, Typeable)

instance E.Exception Failure

data RepliqueState s
  = RepliqueState
  { repliqueHistory   :: [T.Text]
  , repliqueUserState :: s
  }

newtype RepliqueT s m a
  = RepliqueT { unRepliqueT :: (Either E.SomeException a -> RepliqueState s -> m (RepliqueState s)) -- continuation
                            -> RepliqueState s -> m (RepliqueState s) }                             -- result

runRepliqueT  :: (MonadColorPrinter m) => RepliqueT s m a -> s -> m s
runRepliqueT (RepliqueT m) ust =
  let foreverM = m (\r s-> processResult r >> foreverM s) in repliqueUserState <$> foreverM rst
  where
    rst = RepliqueState [] ust
    processResult (Right _) = pure ()
    processResult (Left e) = do
      putDocLn $ fromMaybe renderOtherException tryRenderFailure
      flush
      where
        tryRenderFailure = (\(Failure s)-> template "Failure" $ pretty s) <$> E.fromException e
        renderOtherException = template "Exception" $ pretty (E.displayException e)
        template x y =
             annotate (foreground $ dull Red) $ "*** " <> x <> ": "
          <> annotate (foreground $ bright Red) y

instance (Monad m) => Functor (RepliqueT s m) where
  fmap f fa = RepliqueT $ \cont->
    unRepliqueT fa $ \case
      Left  e -> cont (Left e)
      Right a -> cont (Right (f a))

instance (Monad m) => Applicative (RepliqueT s m) where
  pure a = RepliqueT $ \cont-> cont (Right a)
  fab <*> fa = RepliqueT $ \cont->
    unRepliqueT fa $ \case
      Left  e -> cont (Left e)
      Right a -> unRepliqueT fab $ \case
        Left e' -> cont (Left e')
        Right f -> cont (Right (f a))

instance (Monad m) => Monad (RepliqueT s m) where
  ma >>= k = RepliqueT $ \cont->
    unRepliqueT ma $ \case
      Left  e -> cont (Left e)
      Right a -> unRepliqueT (k a) cont
  fail e = RepliqueT $ \cont-> cont (Left $ E.toException $ Failure e)

instance (Monad m) => MonadFail (RepliqueT s m) where
  fail e = RepliqueT $ \cont-> cont (Left $ E.toException $ Failure e)

lift :: (MonadCatch m) => m a -> RepliqueT s m a
lift ma = RepliqueT $ \cont s->
  try ma >>= \case
    Left  e -> cont (Left e) s
    Right a -> cont (Right a) s

instance (MonadIO m) => MonadIO (RepliqueT s m) where
  liftIO ma = RepliqueT $ \cont s->
    liftIO (E.try ma) >>= \case
      Left  e -> cont (Left e) s
      Right a -> cont (Right a) s

instance (MonadThrow m) => MonadThrow (RepliqueT s m) where
  throwM e = RepliqueT $ \cont->
    cont $ Left $ E.toException e

instance (MonadThrow m) => MonadCatch (RepliqueT s m) where
  catch ma ema = RepliqueT $ \cont->
    unRepliqueT ma $ \case
      Right a -> cont (Right a)
      Left se -> case E.fromException se of
        Just  e -> unRepliqueT (ema e) cont
        Nothing -> cont (Left se)

-- | This instance uses a trick involving an IORef in order to
--   intermediately terminate and evaluate the RepliqueT transformer
--   and then eventually call the continuation or not.
--   The only downside is a restriction on MonadIO, but it is
--   unlikely that anyone will be using this in different context.
instance (MonadIO m, MonadMask m) => MonadMask (RepliqueT s m) where
  mask fma =
    withInterruptedContinuation $ RepliqueT $ \cont s-> mask $ \unmask-> unRepliqueT (fma $ q unmask) cont s
    where
      q unmask ma = withInterruptedContinuation $ RepliqueT (\cont s-> unmask $ unRepliqueT ma cont s)
  uninterruptibleMask fma =
    withInterruptedContinuation $ RepliqueT $ \cont s-> uninterruptibleMask $ \unmask-> unRepliqueT (fma $ q unmask) cont s
    where
      q unmask ma = withInterruptedContinuation $ RepliqueT (\cont s-> unmask $ unRepliqueT ma cont s)

-- | This causes the supplied computation to be called with
--   a terminating fake continuation.
--   The result is then evaluated and eventually the real continuation
--   is called. This is useful for isolating the effect of background
--   state like asynchronous exception masking state.
--   Note: The threaded state is representing all modifications
--   that happended within the supplied computation. This is essentially
--   the most important point of all this effort.
withInterruptedContinuation :: (MonadIO m) => RepliqueT s m a -> RepliqueT s m a
withInterruptedContinuation (RepliqueT action) = RepliqueT $ \cont s-> do
  ref <- liftIO $ newIORef Nothing
  s'  <- action (\r s'-> liftIO (writeIORef ref (Just r)) >> pure s') s
  liftIO (readIORef ref) >>= \case
    Nothing -> pure s'
    Just r  -> cont r s'
{-# INLINE withInterruptedContinuation #-}

instance (MonadCatch m, MonadPrinter m) => T.MonadPrinter (RepliqueT s m) where
  putLn = lift T.putLn
  putChar = lift . T.putChar
  putString = lift . T.putString
  putStringLn = lift . T.putStringLn
  putText = lift . T.putText
  putTextLn = lift . T.putTextLn
  flush = lift T.flush
  getLineWidth = lift T.getLineWidth

instance (MonadCatch m, MonadPrettyPrinter m) => MonadPrettyPrinter (RepliqueT s m) where
  data Annotation (RepliqueT s m) = Annotation' (T.Annotation m)
  putDoc doc = lift $ T.putDoc (reAnnotate (\(Annotation' ann)-> ann) doc)
  setAnnotation (Annotation' a) = lift (T.setAnnotation a)
  resetAnnotation (Annotation' a) = lift (T.resetAnnotation a)
  resetAnnotations = lift T.resetAnnotations

instance (MonadCatch m, MonadPrettyPrinter m, MonadFormatPrinter m) => MonadFormatPrinter (RepliqueT s m) where
  bold       = Annotation' T.bold
  italic     = Annotation' T.italic
  underlined = Annotation' T.underlined

instance (MonadCatch m, MonadPrettyPrinter m, T.MonadColorPrinter m) => T.MonadColorPrinter (RepliqueT s m) where
  inverted     = Annotation' T.inverted
  foreground c = Annotation' (T.foreground c)
  background c = Annotation' (T.background c)

instance (MonadCatch m, MonadTerminal m) => T.MonadTerminal (RepliqueT s m) where
  moveCursorUp                = lift . moveCursorUp
  moveCursorDown              = lift . moveCursorDown
  moveCursorLeft              = lift . moveCursorLeft
  moveCursorRight             = lift . moveCursorRight

  getCursorPosition           = lift   getCursorPosition
  setCursorPosition           = lift . setCursorPosition
  setCursorPositionVertical   = lift . setCursorPositionVertical
  setCursorPositionHorizontal = lift . setCursorPositionHorizontal

  saveCursorPosition          = lift   saveCursorPosition
  restoreCursorPosition       = lift   restoreCursorPosition

  showCursor                  = lift   showCursor
  hideCursor                  = lift   hideCursor

  clearLine                   = lift   clearLine
  clearLineLeft               = lift   clearLineLeft
  clearLineRight              = lift   clearLineRight
  clearScreen                 = lift   clearScreen
  clearScreenAbove            = lift   clearScreenAbove
  clearScreenBelow            = lift   clearScreenBelow

  getScreenSize               = lift   getScreenSize
  useAlternateScreenBuffer    = lift . useAlternateScreenBuffer

instance (MonadCatch m, T.MonadInput m) => T.MonadInput (RepliqueT s m) where
  waitMapInterruptAndEvents = lift . waitMapInterruptAndEvents

instance (Monad m) => MonadQuit (RepliqueT s m) where
  quit = RepliqueT $ \_ s-> pure s

instance (Monad m) => MonadStateful (RepliqueT s m) where
  type State (RepliqueT s m) = s
  load = RepliqueT $ \cont s-> cont (Right $ repliqueUserState s) s
  store x = RepliqueT $ \cont s-> cont (Right ()) s { repliqueUserState = x }

instance (Monad m) => MonadHistory (RepliqueT s m) where
  addToHistory t = RepliqueT $ \cont s-> cont (Right ()) s { repliqueHistory = f (repliqueHistory s) }
    where
      f history
        | not (T.all isSpace t) = t : history
        | otherwise             = history
  searchHistory t = RepliqueT $ \cont s-> cont (Right $ repliqueHistory s) s
