{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Replique.Monad where

import qualified Data.Text as T

class (Monad m) => MonadQuit m where
  quit :: m a

class (Monad m) => MonadStateful m where
  type State m
  load         :: m (State m)
  store        :: State m -> m ()

class (Monad m) => MonadHistory m where
  addToHistory  :: T.Text -> m ()
  searchHistory :: T.Text -> m [T.Text]
