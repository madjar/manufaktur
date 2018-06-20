{-# LANGUAGE NoImplicitPrelude #-}
module Util
  ( readJSON
  ) where

import RIO

import Data.Aeson (FromJSON, eitherDecode')

readJSON ::
  (FromJSON a, MonadReader env m, HasLogFunc env, MonadUnliftIO m) =>
  FilePath -> m a
readJSON f = withLazyFile f (either reportError return . eitherDecode')
  where reportError e = do logError (displayShow e)
                           error e
