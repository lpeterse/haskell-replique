{-# LANGUAGE ScopedTypeVariables #-}
module System.Terminal.Replique (
  -- * Getting started
  -- ** runReplique
    runReplique
  , Replique
  -- ** runRepliqueT
  , runRepliqueT
  , RepliqueT ()
  -- * Reading user input
  -- ** readLine
  , readLine
  , Readable (..)
  -- * Exit handling
  , MonadExit (..)
  , ExitStatus
  , ExitCode (..)
  ) where

import           System.Exit (ExitCode (..))

import           System.Terminal.Replique.Monad
import           System.Terminal.Replique.Readable
import           System.Terminal.Replique.Readline
import           System.Terminal.Replique.RepliqueT

