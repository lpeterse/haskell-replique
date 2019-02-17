module System.Terminal.Replique.Monad where

import qualified Data.Text as T
import           System.Terminal

type family ExitStatus (m :: * -> *) :: *

class (Monad m) => MonadExit m where
  exitWith      :: ExitStatus m -> m a
  catchExit     :: m a -> (ExitStatus m -> m a) -> m a

class (Monad m) => MonadInterrupt m where
  interrupt      :: m a
  catchInterrupt :: m a -> m a -> m a

class (Monad m) => MonadHistory m where
  addToHistory  :: T.Text -> m ()
  searchHistory :: T.Text -> m [T.Text]

class (MonadTerminal m, MonadHistory m, MonadExit m, MonadInterrupt m) => MonadReplique m where
