{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import RIO
import RIO.Process

import Lens.Micro.TH


-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
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


data Manifest = Manifest
  { manifestName :: Text
  , manifestVersion :: Text
  , manifestDependencies :: [(Text, Maybe Text)]
  } deriving (Show)
makeFields ''Manifest
