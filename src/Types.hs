{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import RIO
import RIO.Process

import Data.Aeson (camelTo2)
import Data.Aeson.TH (fieldLabelModifier, deriveJSON, defaultOptions)


-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  , optionsModList :: !FilePath
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  , appCacheDir :: !FilePath
  , appFactorioUsername :: !ByteString
  , appFactorioToken :: !ByteString
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })


data Mod r = Mod
  { modName :: Text
  , modFilename :: FilePath
  , modDownloadUrl :: Text
  , modDependencies :: Maybe ([r], [r])
  } deriving (Show, Functor, Foldable, Traversable, Eq, Ord)

deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3} '' Mod
