{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types where

import RIO
import RIO.Process

import Data.Aeson (FromJSON, ToJSON)


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
  { name :: Text
  , filename :: FilePath
  , downloadUrl :: Text
  , dependencies :: Maybe ([r], [r])
  } deriving (Show, Generic, FromJSON, ToJSON, Functor, Foldable, Traversable, Eq, Ord)
