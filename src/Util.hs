{-# LANGUAGE NoImplicitPrelude #-}
module Util
  ( readJSON
  , reportError
  ) where

import RIO

import Data.Aeson (FromJSON, eitherDecode')

readJSON ::
  (FromJSON a, MonadReader env m, HasLogFunc env, MonadUnliftIO m) =>
  FilePath -> m a
readJSON f = withLazyFile f (either reportError return . eitherDecode')


reportError :: (Show a, MonadReader env m, HasLogFunc env, MonadIO m) => a -> m b
reportError e = do
  logError (displayShow e)
  error (show e)
